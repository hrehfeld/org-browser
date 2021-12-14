(require 'org-ml)


(defun org-util-list-choice-prompt (candidates prompt)
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

(defun org-util-headline-get-title (headline)
  (org-ml-get-property :raw-value headline))

(defun org-util-headline-get-links (headline)
  "Find all links in HEADLINE and return as list."
  (let* ((section (org-ml-headline-get-contents nil headline))
		(title (org-ml-get-property :title headline))
		(link-lists (--map (org-ml-match '(link) it) (cons (org-ml-get-children title) section)))
		(links (apply #'append link-lists)))
	links))

(defun org-util-headline-get-urls (headline)
  "Find all http(s) links in HEADLINE and return as list."
  (let ((valid-link-types '("http" "https")))
	(--map (org-ml-get-property :raw-link it)
		   (--filter (member (org-ml-get-property :type it) valid-link-types)
					 (org-util-headline-get-links headline)))))


(provide 'org-util)
