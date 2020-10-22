;; -*- lexical-binding: t; -*-
(require 'org-ml)
(require 'async)
(require 'dash)
(require 'bindat)
(require 'uuid)

(require 'org-util)


;;TODO: do everything we do on headlines on subtrees instead?

(defvar org-browser-inbox-file (concat org-directory "browser.org") "Org-mode file to put new browser tabs into.")
(defvar org-browser-sync-files nil "Org-mode files to sync browser tabs into.

`org-agenda-files` might be a good choice.")
;;(setq org-browser-sync-files org-agenda-files)
;;(setq org-browser-sync-files (list org-browser-inbox-file))

(defvar org-browser-title-property-name "TITLE" "Name of the property that is used to associate TITLEs to headlines if browser titles are different from the org headline.")
(defvar org-browser-url-property-name "URL" "Name of the property that is used to associate URLs to headlines.")
(defvar org-browser-bookmark-name "bookmark" "Marker to identify headlines that have a bookmark in the browser.

May be used as a tag or property, so be careful to use appropriate characters only.")

(defvar org-browser-open-tab-name "opentab" "Marker to identify headlines that have an open tab in the browser.

May be used as a tag or property, so be careful to use appropriate characters only.")
;; (setq org-browser-open-tab-name "opentab")
(defvar org-browser-open-tab-type 'tag "Where to put the marker that identifies headlines that have an open tab in the browser.

If the symbol 'tag mark the headline with a tag. The Tag name will be `org-browser-open-tab-name`.
If a string mark the headline with a property of that name. The property value will be `org-browser-open-tab-name`.
")
;; (setq org-browser-open-tab-type 'tag)
;; (setq org-browser-open-tab-type "BROWSER")

(defvar org-browser-connection-host 'local "Where to connect to to talk to the browser server program")
(defvar org-browser-connection-port "43893" "Where to connect to to talk to the browser server program")

(defvar org-browser--connection nil "Current connection to the browser server program")
(defvar org-browser--response-handlers nil "Event handlers when responses arrive. Indexed by message_id")


(defun org-browser-hashvalues (hash-table)
	"Return the values of HASH-TABLE as a list"
	(nreverse
	 (let ((res))
		 (maphash (lambda (url tab) (setq res (cons tab res))) hash-table)
		 res)))

(defun org-browser--connection-build-url (what)
  (format "http://%s:%s/%s" org-browser-connection-host org-browser-connection-port what))


;; TODO: rename internal
(defvar org-browser-current-message-size nil)

(defun org-browser--message-id (msg)
  (gethash "imessage" msg))

(defun org-browser--message-payload (msg)
  (gethash "payload" msg))

(defun org-browser-connection-filter (proc raw)
  ;;(message (format  "received: %s (%s)" raw (type-of raw)))
  (let* ((proc-buffer (process-buffer org-browser--connection)))
	(when (= org-browser-current-message-size 0)
	  ;;(raw (encode-coding-string raw 'binary))
	  (setq org-browser-current-message-size (org-browser--unpack-u32 raw))
	  ;; ignore/cut-off msg-size bytes
	  (setq raw (substring-no-properties raw 4)))
	(with-current-buffer proc-buffer
	  (insert raw)
	  ;;(message "%s vs. %s" (buffer-size) org-browser-current-message-size)
	  (when (>= (buffer-size) org-browser-current-message-size)
		(let* ((temp-buffer (get-buffer-create (make-temp-name "*org-browser-message-")))
			   (message-end (1+ org-browser-current-message-size))
			   (serialized (decode-coding-region (point-min) message-end 'utf-8 temp-buffer)))
		  (delete-region (point-min) message-end)
		  (setq org-browser-current-message-size 0)
		  (let* ((msg (with-current-buffer temp-buffer (json-parse-buffer))))
				(kill-buffer temp-buffer)
				(let* ((message-id (org-browser--message-id msg))
							 (payload (org-browser--message-payload msg))
							 (handler (cdr (assoc message-id org-browser--response-handlers))))
					;;(message "Received response: %s %S" message-id payload)
					;;(message "%S: %S" message-id msg)
					;;(message "%S" org-browser--response-handlers)
					(assert handler)
					(setq org-browser--response-handlers (assoc-delete-all message-id org-browser--response-handlers))
					(funcall handler payload))
			  ))))))


(defun org-browser-connection-sentinel (proc msg)
  (message "Sentinel: %s" msg )
  (cond
   ((member msg '("connection broken by remote peer\n" "deleted\n"))
	(org-browser-connection-delete))
   (t (message (format "=====UNKNOWN sentinel message: %s" msg))
	  (debug)
	  (org-browser-connection-delete))))

(defun org-browser-connection-open ()
  (org-browser-connection-delete)
  (let* ((name (make-temp-name "org-browser-connection-"))
		 (proc-buffer (get-buffer-create (concat "*" name "*")))
		 (proc (make-network-process :name name :buffer proc-buffer
									 :host org-browser-connection-host :service org-browser-connection-port
									 ;;:coding 'binary
									 )))
	(set-process-filter-multibyte proc nil)
	;;(set-process-coding-system proc 'utf-8 'utf-8)
	(set-process-coding-system proc 'binary 'binary)
	(set-process-sentinel proc #'org-browser-connection-sentinel)
	(set-process-filter proc #'org-browser-connection-filter)
	(setq org-browser--connection proc)
	(setq org-browser-current-message-size 0
		  org-browser--response-handlers nil)
	proc
	))
;;(org-browser-connection-open)
;;(global-set-key (kbd "<f7>") (lambda () (interactive) (org-browser-connection-open)))


(defun org-browser-connection-get ()
  (unless org-browser--connection (org-browser-connection-open))
  org-browser--connection)
;; (org-browser-connection-get)

(defun org-browser-connection-delete ()
  "Low-level send"
  (when org-browser--connection
	(kill-buffer (process-buffer org-browser--connection))
	;;(delete-process org-browser--connection)
	(setq org-browser--connection nil)
	))
(global-set-key (kbd "<f7>") (lambda () (interactive) (org-browser-connection-delete)))

(defun org-browser-connection-send-raw (msg)
  "Send a json encoded MSG after making sure connection is up."
  (let ((proc (org-browser-connection-get))
		(raw-msg (json-encode msg)))
	;; send byte-length of payload as binary
	(process-send-string org-browser--connection (org-browser--pack-u32 (string-bytes raw-msg)))
	(process-send-string org-browser--connection raw-msg)
	))


(defun org-browser-connection-send (payload handler)
  (let* ((imessage (uuid-string))
				 (msg `((payload . ,payload) (imessage . ,imessage))))
	(setq org-browser--response-handlers
				(append (list (cons imessage handler))
								org-browser--response-handlers))
	(org-browser-connection-send-raw msg)
	))


(defun org-browser--pack-u32 (num)
  (bindat-pack '((type u32)) `((type . ,num))))
;;(org-browser--pack-u32 42)

(defun org-browser--unpack-u32 (bytes)
  (cdar (bindat-unpack '((type u32)) bytes)))
;;(org-browser--unpack-u32 (org-browser--pack-u32 42))


(defun org-browser-query-status (tab-url handler)
  (org-browser-connection-send
	 `((type . status)
		 (url . ,tab-url))
	 handler))
;;(global-set-key (kbd "<f10>") (lambda () (interactive) (org-browser-query-status "https://github.com/tkf/emacs-request")))
;;(json-serialize '((:url . "https://github.com/tkf/emacs-request")))


(defun org-browser-query-set-status (tab-url status title handler)
  (org-browser-connection-send
	 `((type . set)
		 (status . ,status)
		 (title . ,title)
		 (url . ,tab-url))
	 handler))


(defun org-browser-query-all  (callback)
  (org-browser-connection-send
   `((type . all))
   callback))


(defun org-browser-query-activate (url callback)
  (org-browser-connection-send
   `((type . activate)
		 (url . ,url))
   callback))


;;(defun org-browser-headline-select-url (headline))

(defun org-browser-filter-headline-with-uuid (uuid headlinesa)
  (cl-flet ((has-this-id (headline)
												 (equal uuid
																(org-ml-headline-get-node-property "ID" headline))))
		(save-excursion
			(let* ((headlines (org-ml-match '(headline) headlinesa))
						 (matching-headlines (-filter #'has-this-id headlines)))
				(assert matching-headlines t "No headline with this uuid!" uuid headlines headlinesa)
				(if (cdr matching-headlines)
						(error (format "Multiple matching headlines found for UUID %S" uuid))
					(car matching-headlines))))))


(defun org-browser-update (old new)
	(save-excursion
		(let ((begin (org-ml-get-property :begin old))
					(end (org-ml-get-property :end old)))
			(message "Updating from (%s %s) %s" begin end (org-ml-get-property :raw-value new))
			(assert begin)
			(assert end)
			(delete-region begin end)
			(goto-char begin)
			(insert (org-element-interpret-data new)))))

(defun org-browser-kill (headline)
	(save-excursion
		(let ((begin (org-ml-get-property :begin headline))
					(end (org-ml-get-property :end headline)))
			(assert begin)
			(assert end)
			(delete-region begin end))))

(defun org-browser-with-headline-by-id (headline-buffer headline-id f)
	(with-current-buffer headline-buffer
		(save-excursion
			(let* ((old-headline (->> (org-ml-parse-this-buffer)
																(org-browser-filter-headline-with-uuid headline-id))))
				(let ((headline (funcall f old-headline)))
					(when headline
						(org-browser-update old-headline headline)))))))


(defun org-browser-headline-url-interactively (headline-buffer headline)
	(or (org-ml-headline-get-node-property org-browser-url-property-name headline)
			(let* ((urls (org-util-headline-get-urls headline))
						 (url (org-util-list-choice-prompt
									 urls
									 (format "No existing %s property, but body URLs found. Choose which URL this headline represents: "
													 org-browser-url-property-name))))
				(when url
					;; set URL property to selected URL
					(org-ml-update* (org-ml-headline-set-node-property org-browser-url-property-name url it) headline))
				url)))


(defun org-browser-sync-headline-handler (kill-headline headline-buffer headline-id tabs)
	"Handle the result TABS of a sync-headline call for a headline with HEADLINE-ID in HEADLINE-BUFFER.

If KILL-HEADLINE is non-nil, kill the headline if it has no in-browser representation."
	(case (length tabs)
		(0
		 (org-browser-with-headline-by-id
			headline-buffer
			headline-id
			(lambda (headline)
				(message "Nothing found: %s" (org-browser-headline-url headline))
				(if kill-headline
						(org-browser-kill headline)
					(when (org-browser-headline-status headline)
						(org-browser-headline-set-status nil headline))))))
		(t (let ((tab (seq-first tabs)))
				 (org-browser-with-headline-by-id
					headline-buffer
					headline-id
					(lambda (headline)
						(message "%s found: %s"
										 (capitalize (symbol-name (org-browser-tab-status tab)))
										 (org-browser-headline-url headline))
						(if kill-headline
								(org-browser-kill headline)
							(let* ((status (org-browser-tab-status tab))
										 (title (org-browser-tab-title-escaped tab))
										 )
								(org-browser-headline-set-interactively headline-buffer title status nil headline))
						)))))
		;; TODO not sure if this might be an error
		;;(t (error "More than one browser representation for URL %s found: %S" url tabs))
		))


(defun org-browser-sync-headline (headline-buffer headline)
  "Sync browser tab/bookmark status for the headline at point"
  (let ((headline-id (org-ml-headline-get-node-property "ID" headline))
				(url (org-browser-headline-url-interactively headline-buffer headline)))
		(unless url
			(error "No URL on this headline"))
		(let ((url (url-normalize-url url)))
			(message "Checking status of %s." url)
			(org-browser-query-status
			 url
			 ;; async
			 (lambda (tabs)
				 (org-browser-sync-headline-handler nil headline-buffer headline-id tabs))))))

(defun org-browser-this-headline ()
  "Get the headline at point, creating ID"
  (interactive)
  (let* ((headline-id (org-id-get-create))
				 (headline (org-ml-parse-this-headline)))
		headline))

(defun org-browser-sync-this-headline ()
  "Sync browser tab/bookmark status for the headline at point"
  (interactive)
  (org-browser-sync-headline (current-buffer) (org-browser-this-headline)))
(global-set-key (kbd "<f9>") #'org-browser-sync-this-headline)



(defun org-browser-headline-sync-status (status headline-buffer headline)
  "Sync browser tab/bookmark status for the headline at point"
  (let ((headline-id (org-ml-headline-get-node-property "ID" headline))
				(title (org-util-headline-get-title headline))
				(url (org-browser-headline-url-interactively headline-buffer headline)))
		(unless url
			(error "No URL on this headline"))
		(let ((url (url-normalize-url url))
					(kill-headline (eq status 'kill-trash)))
			(org-browser-query-set-status
			 url
			 ;; kill-trash is just kill for the browser
			 (if kill-headline 'kill status)
			 title
			 ;; async
			 (lambda (tabs)
				 (org-browser-sync-headline-handler kill-headline headline-buffer headline-id tabs)
				 ;; (when (= 1 (length tabs))
				 ;; 	 (let ((tab (seq-first tabs)))
				 ;; 		 (message "Set status %S for %s" status (org-browser-tab-title tab))
				 ;; 		 (org-browser-with-headline-by-id
				 ;; 			headline-buffer
				 ;; 			headline-id
				 ;; 			(lambda (headline)
				 ;; 				(if (eq status 'kill-trash)
				 ;; 						(progn
				 ;; 							(org-browser-kill headline)
				 ;; 							;; important: return nil so the wrapping org-browser-with-headline-by-id doesn't
				 ;; 							;; try to update the headline when this returns
				 ;; 							nil)
				 ;; 					;; anything but kill-trash potentially modifies the headline
				 ;; 					(let* ((status (case status (kill nil) (kill-trash nil) (t status)))
				 ;; 								 (old-status (org-browser-headline-status headline)))
				 ;; 						(when (not (eq status old-status))
				 ;; 							(org-browser-headline-set-status status headline))))))
				 ;; 		 ))
				 )))))

(defun org-browser-this-headline-sync-status (&optional status)
  "Sync browser tab/bookmark status for the headline at point"
  (interactive)
	(unless status
		(let ((read-answer-short nil)
					(action (read-answer (format "Set headline status: ")
															 '(("tab"  ?t "Open as Tab")
																 ("bookmark"   ?b "Save as Bookmark")
																 ("kill"  ?k "Remove from browser")
																 ("kill-trash"  ?K "Remove from browser & Cut headline")
																 ))))
			(setq status (intern action))))
	(let ((headline (org-browser-this-headline)))
		(org-browser-headline-sync-status status
																			(current-buffer)
																			headline)))
(define-key my-tools-command-keymap (kbd "c") #'org-browser-this-headline-sync-status)


(defun org-browser-headline-activate (headline-buffer headline)
  "Focus HEADLINE from HEADLINE-BUFFER in browser."
  (let ((headline-id (org-ml-headline-get-node-property "ID" headline))
				(title (org-util-headline-get-title headline))
				(url (org-browser-headline-url-interactively headline-buffer headline)))
		(unless url
			(error "No URL on this headline"))
		(let ((url (url-normalize-url url)))
			(org-browser-query-activate
			 url
			 ;; async
			 (lambda (tabs)
				 (unless (= 1 (length tabs))
					 (error "Error activating tab for URL %s found: %s" url tabs))
				 )))))

(defun org-browser-this-headline-activate ()
  "Sync browser tab/bookmark status for the headline at point"
  (interactive)
	(let ((headline (org-browser-this-headline)))
		(org-browser-headline-activate (current-buffer) headline)))
(define-key my-tools-command-keymap (kbd "a") #'org-browser-this-headline-activate)

(defun org-browser-headline-title-escape (title)
	(->> title
			 ;; remove * because of headline level syntax
			 (replace-regexp-in-string "^[*]+[:blank:]" "")
			 ;; remove : because of tag syntax
			 (replace-regexp-in-string "[:blank:]*\\(:[^:[:blank:]]+\\)+:[:blank:]*$" "")
			 ;; remove leading numbers 1. because of numbered lists
			 (replace-regexp-in-string "^[0-9]+\\.[ 	]+" "")
			 ))

;;(org-browser-headline-title-escape "** 3. Python: foo :foo:bar:")

(defun org-browser-headline-set (title status url headline)
	(->> headline
			 (org-browser-headline-set-title title)
			 (org-browser-headline-set-status status)
			 (org-browser-headline-set-url url)
			 ))

(defun org-browser-headline-needs-update (title title-prop status url  headline)
	(let ((title (or title (org-util-headline-get-title headline)))
				;; title-prop has a legal value of nil
				;; status has a legal value of nil
				(url (or url (org-browser-headline-url headline))))
		;; go through all properties and see if they need updating
		(or
		 (not (string-equal title (org-util-headline-get-title headline)))
		 (not (string-equal title-prop (org-browser-headline-title-prop headline)))
		 (not (eq status (org-browser-headline-status headline)))
		 (not (string-equal url (org-browser-headline-url headline)))
		 )))


(defun org-browser-headline-set-status (status headline)
	(let ((status-token (xcond
											 ((eq status 'tab) org-browser-open-tab-name)
											 ((eq status 'bookmark) org-browser-bookmark-name)
											 ((eq status nil) nil))))
		(cl-flet* ((tags-deleter ()
														 (->> headline
																	(org-ml-remove-from-property :tags org-browser-open-tab-name)
																	(org-ml-remove-from-property :tags org-browser-bookmark-name)))
							 (tags-setter (status-token)
														(tags-deleter)
														(->> headline
																 (org-ml-insert-into-property :tags 0 status-token)))
							 (property-deleter ()
																 (org-ml-headline-set-node-property org-browser-open-tab-type "" headline))
							 (property-setter (status-token)
																(property-deleter)
																(org-ml-headline-set-node-property org-browser-open-tab-type status-token headline)))
			(let ((setter (if (eq org-browser-open-tab-type 'tag)
												#'tags-setter
											#'property-setter))
						(deleter (if (eq org-browser-open-tab-type 'tag)
												 #'tags-deleter
											 #'property-deleter)))
				(if status-token
						(funcall setter status-token)
					(funcall deleter))))))

(defun org-browser-headline-set-title (title headline)
	(org-ml-headline-set-title! (org-browser-headline-title-escape title) nil headline))

(defun org-browser-headline-set-title-prop (title-prop headline)
	(org-ml-headline-set-node-property org-browser-title-property-name title-prop headline))

(defun org-browser-headline-set-url (url headline)
	(org-ml-headline-set-node-property org-browser-url-property-name url headline))

(defun org-browser-headline-check-title-interactively (new-title buffer headline)
	"Return "
  (let ((old-title (org-util-headline-get-title headline))
				(title-prop (org-browser-headline-title-prop headline)))
		(if (or (string-empty-p old-title)
						(string-equal new-title old-title)
						title-prop
						)
			(cons title-prop new-title)
			(with-current-buffer buffer
				(save-excursion
					(let ((headline-pos (org-ml-get-property :begin headline)))
						(assert headline-pos)
						;; position to have a look at the headline for the prompt
						(goto-char headline-pos))
					(let* (;;(read-answer-short t)
								 (action (intern
													(read-answer (format "Overwrite Headline Title? \"%s\" => \"%s\": " old-title new-title)
																			 '(("overwrite"  ?o "overwrite with tab title")
																				 ("keep"   ?k "keep org title")
																				 ("edit"  ?e "edit tab title and overwrite"))))))

						(case action
							('edit
							 (let ((title (read-string "Edit new title: " new-title)))
								 (cons (unless (string-equal old-title title) new-title) title)))
							('keep
							 (cons new-title old-title))
							('overwrite
							 (cons nil new-title))
							(t (assert nil)))))))))


(defun org-browser-headline-set-interactively (headline-buffer title status url headline)
	"Set TITLE, STATUS and URL of HEADLINE, possibly querying the user in the process. Return modified headline or nil if nothing changed."
	(let* ((url (or url (org-browser-headline-url headline)))
				 (title (org-browser-headline-check-title-interactively title headline-buffer headline))
				 (title-prop (car title))
				 (title (cdr title)))
		;; return headline only if changed
		(when (org-browser-headline-needs-update title title-prop status url headline)
			(->> (org-browser-headline-set-title-prop title-prop headline)
					 (org-browser-headline-set title status url)))))


(defun org-browser-tab-url (tab)
  (gethash "url" tab))

(defun org-browser-tab-title (tab)
  (gethash "title" tab))

(defun org-browser-tab-title-escaped (tab)
  (org-browser-headline-title-escape (org-browser-tab-title tab)))

(defun org-browser-tab-status (tab)
	"Return the status of the TAB as a symbol.

Should be either 'tab or 'bookmark"
	;;fixme type or status?
	(let ((status (gethash "type" tab)))
		(if (string-equal status "kill")
				nil
			(intern status))))

(defun org-browser-headline-title-prop (headline)
  (org-ml-headline-get-node-property org-browser-title-property-name headline))

(defun org-browser-headline-url (headline)
  (org-ml-headline-get-node-property org-browser-url-property-name headline))

(defun org-browser-headline-status (headline)
	(if (eq org-browser-open-tab-type 'tag)
			(let ((tags (org-ml-get-property :tags headline)))
				(cond
				 ((member org-browser-open-tab-name tags) 'tab)
				 ((member org-browser-bookmark-name tags) 'bookmark))
				;;nil for not opened/saved/represented in browser
				)
		(org-ml-headline-get-node-property org-browser-open-tab-type headline)))

(defun org-browser-node-without-parent (node)
	(org-plist-delete (cdr node) :parent))

(defun org-browser-sync ()
  (interactive)
  (org-browser-query-all
   (lambda (item-list)
		 ;;(message "%s tabs received" (length item-list))
		 ;;(message "%S" item-list)
		 (cl-flet ((put-tab (tab table) (puthash (url-normalize-url (org-browser-tab-url tab)) tab table))
							 (get-tab (url table) (gethash (url-normalize-url url) table)))
			 (let ((tabs-map (make-hash-table :test 'equal))
						 (found-tabs (make-hash-table :test 'equal)))
				 ;; add items to the tabs-map by url
				 (seq-doseq (tab item-list)
					 (put-tab tab tabs-map))

				 (save-window-excursion
					 (let ((sync-files (append org-browser-sync-files (list org-browser-inbox-file))))
						 (dolist (file-name sync-files)
							 (let ((curbuf (find-file-noselect file-name)))
								 (with-current-buffer curbuf
									 (let* ((headlines (->> (org-ml-parse-this-buffer)
																					(org-ml-match '(headline))))

													(headlines (let (;; retain headlines that have an open browser tab
																					 (browser-headlines (--filter (get-tab (org-browser-headline-url it) tabs-map)
																																				headlines))
																					 ;; headlines that were closed since the last update
																					 ;; have an url set, still have a tab/bookmark marker and are not the list of tabs
																					 (closed-headlines
																						(--filter
																						 (let ((url? (org-browser-headline-url it))
																									 (opened? (org-browser-headline-status it)))
																							 (and url? opened?
																										(not (get-tab url? tabs-map))))
																						 headlines)))
																			 ;; (message "items: %s" (length item-list))
																			 ;; ;;(print (--map (org-browser-tab-url it) item-list))
																			 ;; (message "headlines: %s" (length headlines))
																			 ;; ;;(print (--map (org-browser-headline-url it) headlines))
																			 ;; (message "matched: %s" (length browser-headlines))
																			 ;; ;;(print (--map (org-browser-headline-url it) browser-headlines))
																			 ;; (message "closed: %s" (length closed-headlines))
																			 ;; ;;(print (--map (org-browser-headline-url it) closed-headlines))
																			 (append (--map (cons 'kill it) closed-headlines)
																							 (--map (cons 'update it) browser-headlines))
																			 )))
										 ;;(message "matched-closed: %s" (length headlines))
										 ;;(print (--map (org-browser-headline-url it) headlines))

										 (cl-flet* ((get-begin (it) (org-ml-get-property :begin (cdr it)))
																(by-begin (a b) (< (get-begin a) (get-begin b))))
											 ;; reverse sort so we go bottom-to-top through the headlines
											 (setq headlines (-sort (lambda (a b) (not (by-begin a b)))
																							headlines))
											 ;(print (--map (org-browser-headline-url (cdr it)) headlines))
											 )
										 ;; because we reversed earlier (we go bottom to top through the headlines)
										 ;; we can just update headlines with the originally parsed begin/end
										 ;; markers. Otherwise, we would need to take care that changes to previous
										 ;; headlines are applied to each begin/end marker.
										 ;;(message "matched-closed: %s" (length headlines))
										 (dolist (it headlines)
											 (let ((headline (cdr it))
														 (op (car it)))
												 (let ((updated-headline ; will be nil if not changed
																(if (eq op 'kill)
																		;; closed headline (headline only in org)
																		;; update all of the unclosed headlines that were now closed in the browser
																		(when (org-browser-headline-status headline)
																			(org-browser-headline-set-status nil headline))
																	;; headline with browser representation -- update headline
																	(progn (assert (eq op 'update))
																				 (let ((tab (get-tab (org-browser-headline-url headline) tabs-map)))
																					 (put-tab tab found-tabs)
																					 (let* (
																									(title (org-browser-tab-title-escaped tab))
																									;;(url (org-browser-tab-url tab))
																									(url (org-browser-headline-url headline))
																									(status (org-browser-tab-status tab))
																									)
																						 ;;(message "Found tab for %S" url)
																						 ;;(message "Found headline for %s in %s" headline-url file-name)
																						 ;; return headline only if changed
																						 (org-browser-headline-set-interactively curbuf title status nil headline))
																					 )))))
													 ;; update if present
													 (when updated-headline
														 (org-browser-update headline updated-headline))))))
									 ))))
					 (let ((inbox-buf (find-file-noselect org-browser-inbox-file)))
						 (with-current-buffer inbox-buf
							 (save-excursion
								 (cl-flet ((url-found (url) (get-tab url found-tabs)))
									 (->> (org-browser-hashvalues tabs-map)
												;; tabs whose url is not in found-tabs
												(--filter (not (url-found (org-browser-tab-url it))))
												(-map (lambda (tab)
																(let ((title (org-browser-tab-title-escaped tab))
																			(url (org-browser-tab-url tab))
																			(status (org-browser-tab-status tab)))
																	;(message "Creating for url: (%S --------- %S)" url (url-normalize-url url))
																	(->> (org-ml-build-headline)
																			 (org-browser-headline-set title status url)
																			 (org-ml-insert-tail (point-max))))
																(org-id-get-create)))
												))

								 (maphash (lambda (url tab)
														(unless (get-tab url found-tabs)
															))
													tabs-map)))))))
		 )))
(global-set-key (kbd "<f8>") #'org-browser-sync)
;;(setq print-length 999)

(defun org-browser-tag-hook-fun ()
	())
(add-hook 'org-after-tags-change-hook #'org-browser-tag-hook-fun)

(defun org-browser-cut-subtree ()
	"Cut the subtree point is on and remove from browser."
	(interactive)
	(org-browser-this-headline-sync-status 'kill-trash))

(substitute-key-definition #'org-cut-subtree #'org-browser-cut-subtree org-mode-map)
(provide 'org-browser)
