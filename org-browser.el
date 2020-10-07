;; -*- lexical-binding: t; -*-
(require 'org-ml)
(require 'async)
(require 'dash)
(require 'request)
(require 'bindat)
(require 'uuid)

(defvar org-browser-inbox-file (concat org-directory "browser.org") "Org-mode file to put new browser tabs into.")
(defvar org-browser-sync-files nil "Org-mode files to sync browser tabs into.

`org-agenda-files` might be a good choice.")
;;(setq org-browser-sync-files org-agenda-files)

(defvar org-browser-url-property-name "URL" "Name of the property that is used to associate URLs to headlines.")

(defvar org-browser-connection-host 'local "Where to connect to to talk to the browser server program")
(defvar org-browser-connection-port "43893" "Where to connect to to talk to the browser server program")

(defvar org-browser--connection nil "Current connection to the browser server program")
(defvar org-browser--response-handlers nil "Event handlers when responses arrive. Indexed by message_id")

(defun org-browser--connection-build-url (what)
  (format "http://%s:%s/%s" org-browser-connection-host org-browser-connection-port what))


(defun org-browser-connection-filter (proc raw)
  ;;(message (format  "received: %s (%s)" raw (type-of raw)))
  (let* ((raw (decode-coding-string raw 'binary))
		 (msg-size (org-browser--unpack-u32 raw))
		 (raw (substring raw 4))
		 (msg (encode-coding-string raw 'utf-8))
		 )
	;;(message "%s: %s" msg-size msg)
	(let* ((msg (json-read-from-string msg))
		   (message-id (cdr (assq 'imessage msg)))
		   (payload (cdr (assq 'payload msg)))
		   (handler (cdr (assoc message-id org-browser--response-handlers))))
	  (setq org-browser--response-handlers (assoc-delete-all message-id org-browser--response-handlers))
	  (message "Received response: %s" message-id)
	  (funcall handler payload)
	  )
	)
  )


(defun org-browser-connection-sentinel (proc msg)
  (message "Sentinel: %s" msg )
  (cond
   ((string= msg "connection broken by remote peer\n")
	(org-browser-connection-delete))
   (t (message (format "=====UNKNOWN sentinel message: %s" msg))
	  (debug)
	  (org-browser-connection-delete))))

(defun org-browser-connection-open ()
  (let* ((name (make-temp-name "org-browser-connection-"))
		 (proc-buffer (get-buffer-create (concat "*" name "*")))
		 (proc (make-network-process :name name :buffer proc-buffer
									 :host org-browser-connection-host :service org-browser-connection-port
									 ;;:coding 'binary
									 )))
	;;(set-process-filter-multibyte proc t)
	(set-process-coding-system proc 'utf-8 'utf-8)
	;;(set-process-coding-system proc 'binary 'binary)
	(set-process-sentinel proc #'org-browser-connection-sentinel)
	(set-process-filter proc #'org-browser-connection-filter)
	(setq org-browser--connection proc)
	proc
	))

(defun org-browser-connection-get ()
  (unless org-browser--connection (org-browser-connection-open))
  org-browser--connection)
;; (org-browser-connection-get)

(defun org-browser-connection-delete ()
  "Low-level send"
  (delete-process org-browser--connection)
  (setq org-browser--connection nil))
;; (org-browser-connection-delete)

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
	(message "new message: %s" imessage)
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


(defun org-browser-url-is-opened (tab-url handler)
  (message "Syncing %s." tab-url)
  (org-browser-connection-send `((type . status)
								 (url . ,tab-url))
							   (lambda (item-list)
								 (if (not (seq-empty-p item-list))
									 (let* ((item (elt item-list 0))
											(type (assq 'type item)))
									   (message "Received response: %S" item)
									   (funcall handler item)
									   )
								   (funcall handler nil)
								   ))))
;;(global-set-key (kbd "<f10>") (lambda () (interactive) (org-browser-url-is-opened "https://github.com/tkf/emacs-request")))
;;(json-serialize '((:url . "https://github.com/tkf/emacs-request")))

(defun org-browser-url-open (tab-url handler)
  (org-browser-connection-send `((type . open)
								 (url . ,tab-url))
							   (lambda (item-list)
								 (if (not (seq-empty-p item-list))
									 (let* ((item (elt item-list 0))
											(type (assq 'type item)))
									   (message "Received response: %S" item)
									   (funcall handler item)
									   )
								   (error "Something went wrong when opening tab %s." tab-url)
								   ))))

(defun org-browser--body-element-is-link (el)
  (and (listp el) (eq (car el) 'link)))

(defun org-browser--body-link-get-raw-link (link-el)
  (let* ((link-data (cadr link-el))
		 (raw-link (plist-get link-data :raw-link)))
	raw-link))

(defun org-browser--body-link-get-all (els)
  (mapcar #'org-browser--body-link-get-raw-link
		  (-filter #'org-browser--body-element-is-link els)))

(defun org-browser-headline-get-links (headline)
  "Find all links in HEADLINE and return as list."
  (let* ((section (org-ml-headline-get-contents t t headline))
		(title (org-ml-get-property :title headline))
		(link-lists (--map (org-ml-match '(link) it) (cons (org-ml-get-children title) section)))
		(links (apply #'append link-lists)))
	links))

(defun org-browser-headline-get-urls (headline)
  "Find all http(s) links in HEADLINE and return as list."
  (let ((valid-link-types '("http" "https")))
	(--map (org-ml-get-property :raw-link it)
		   (--filter (member (org-ml-get-property :type it) valid-link-types)
					 (org-browser-headline-get-links headline)))))

;;(defun org-browser-headline-select-url (headline))

(defun org-browser-filter-headline-with-uuid (uuid headlines)
  (cl-flet ((has-this-id (headline)
						 (equal uuid
								(org-ml-headline-get-node-property "ID" headline))))
	(save-excursion
	  (let ((matching-headlines
			 (->> headlines
				  (org-ml-match '(headline))
				  (-filter #'has-this-id))))
		(if (cdr matching-headlines)
			(error (format "Multiple matching headlines found for UUID %S" uuid))
		  (car matching-headlines))))))


(defun org-browser-headline-get-title (headline)
  (substring-no-properties (car (org-ml-get-property :title headline))))


(defun org-browser-list-choice-prompt (candidates prompt)
  "Select one choice from CANDIDATES.

If CANDIDATES is empty, return nil.
If CANDIDATES has one element, return that element.
Otherwise PROMPT the user for a choice."
  (when candidates
	  (cond
	 ((not (cdr candidates))
	  (car candidates))
	 (t
	  (completing-read prompt candidates nil t (car candidates))))))


(defun org-browser-tab-title (tab)
  (cdr (assq 'title tab)))

(defun org-browser-sync-headline ()
  "Sync browser tab/bookmark status for the headline at point"
  (interactive)
  (save-excursion
	(let* ((headline-id (org-id-get-create))
		   (headline (org-ml-parse-this-headline))
		   ;; TODO: extract org-browser-headline-select-url, but what's elegant to still have update-headline-url?
		   (url (or (org-ml-headline-get-node-property org-browser-url-property-name headline)
					(let* ((urls (org-browser-headline-get-urls headline))
						   (url (org-browser-list-choice-prompt
								 urls
								 (format "No existing %s property, but body URLs found. Choose which URL this headline represents: "
										 org-browser-url-property-name))))
					  (when url
						;; set URL property to selected URL
						(save-excursion
						  (org-ml-update* (org-ml-headline-set-node-property org-browser-url-property-name url it) headline)))
					  url))))
	  (cl-flet ((handle-url-found (tab)
								  (let* ((headline (org-browser-filter-headline-with-uuid headline-id (org-ml-parse-this-buffer)))
										 (headline-title (save-excursion (org-browser-headline-get-title headline)))
										 (title (org-browser-tab-title tab))
										 (title (when (not (string-equal title headline-title))
												  (let ((read-answer-short t)
														(action (read-answer (format "Overwrite Headline Title? \"%s\" -> \"%s\": " headline-title title)
																			 '(("yes"  ?y "overwrite with tab title")
																			   ("no"   ?n "keep org title")
																			   ("edit"  ?e "edit tab title and overwrite")))))
													(xcond
													 ((string-equal action "edit")
													  (read-string "Edit new title: " title))
													 ((string-equal action "no")
													  nil)
													 (t title))))))
									(save-excursion
									  (when title
										(org-ml-update* (org-ml-headline-set-title! title nil it) headline))
									  (org-ml-update* (org-ml-insert-into-property :tags 0 "browsertab" it) headline)))))
		(if url
			(org-browser-url-is-opened
			 (url-normalize-url url)
			 (lambda (tab)
			   (if tab
				   (handle-url-found tab)
				 (progn
				   (message "No tab for URL %s found." url)
				   ;;(org-browser-url-open url #'handle-url-found)
				   )
				 )))
		  (error "No URL on this headline"))))))
;;(global-set-key (kbd "<f9>") #'org-browser-sync-headline)

(provide 'org-browser)
