(require 'ox)
(require 'ox-publish)
(require 'ox-ascii)
(require 'cl-lib)


;; TODO:
;; Sublists aren't supported in gemini
;; There's a trailing space after inline code samples

(org-export-define-derived-backend 'gemini 'ascii
  :menu-entry
  '(?g "Export to Gemini"
       ((?b "To buffer"
	    (lambda (a s v b)
	      (org-gemini-export-to-buffer a s v b nil)))
	(?f "To file"
	    (lambda (a s v b)
	      (org-gemini-export-to-file a s v b nil)))
	))
  :translate-alist '(
		     (code . org-gemini-code-inline) 
		     ;; (export-block . org-gemini-identity)

		     ;; (center-block . org-gemini-identity)
		     ;; (code . org-gemini-identity)
		     ;; (drawer . org-gemini-identity)
		     ;; (dynamic-block . org-gemini-identity)
		     ;; (example-block . org-gemini-identity)
		     ;; (export-block . org-gemini-identity)
		     ;; (fixed-width . org-gemini-identity)
		     (headline . org-gemini-headline)
		     ;; (horizontal-rule . org-gemini-identity)
		     ;; (inline-src-block . org-gemini-code-inline)
		     ;; (inlinetask . org-gemini-identity)
		     ;; (inner-template . org-gemini-identity)
		     ;; (italic . org-gemini-identity)
		     ;; (item . org-gemini-identity)
		     ;; (keyword . org-gemini-identity)
		     ;; (line-break . org-gemini-identity)
		     (link . org-gemini-link)
		     ;; (node-property . org-gemini-identity)
		     ;; (paragraph . org-gemini-identity)
		     ;; (plain-list . org-gemini-identity)
		     ;; (plain-text . org-gemini-identity)
		     ;; (property-drawer . org-gemini-identity)
		     ;; (quote-block . org-gemini-identity)
		     (section . org-gemini-section)
		     ;; (special-block . org-gemini-identity)
		     (src-block . org-gemini-code-block)
		     ;; (table . org-gemini-identity)
		     (template . org-gemini-template)
		     ;; (verbatim . org-gemini-identity 
		     )
  )

(defun org-gemini-paragraph (paragraph contents info)
  (progn
    (message "we were at least called in the paragraph function")
    paragraph))

(defun org-gemini-identity (input contents info)
  "this is a test")

(defun org-gemini-code-inline (input contents info)
  ;; there's a bug here where there's a trailing space in the ``
  (format "`%s`" (org-export-format-code-default input info)))

(defun org-gemini-code-block (example-block _contents info)
  (org-remove-indentation
   (format "```\n%s```"
	   (org-export-format-code-default example-block info))))

(defun org-gemini--describe-links (links width info)
  (mapconcat
   (lambda (link)
     (let* ((path (org-element-property :raw-link link))
	    (desc (org-element-contents link))
	    (anchor (org-export-data
		     (or desc (org-element-property :raw-link link))
		     info))
	    )
       (format "=> %s %s\n" path anchor)))
   links ""))


(defun org-gemini-link (link desc info)
  (if (org-string-nw-p desc)
      (format "[%s]" desc)))


(defun org-gemini-section (section contents info)
  "Transcode a SECTION element from Org to ASCII.
CONTENTS is the contents of the section.  INFO is a plist holding
contextual information."
  (let ((links
	 (and (plist-get info :ascii-links-to-notes)
	      ;; Take care of links in first section of the document.
	      (not (org-element-lineage section '(headline)))
	      (org-gemini--describe-links
	       (org-ascii--unique-links section info)
	       (org-ascii--current-text-width section info)
	       info))))
    (org-remove-indentation
      (if (not (org-string-nw-p links)) contents
	(concat (org-element-normalize-string contents) "\n\n" links))
      ;; Do not apply inner margin if parent headline is low level.
      (let ((headline (org-export-get-parent-headline section)))
	(if (or (not headline) (org-export-low-level-p headline info)) 0
	  (plist-get info :ascii-inner-margin))))))

(defun org-gemini--build-title
    (element info text-width &optional underline notags toc)
  (let ((number (org-element-property :level element))
	(text
	 (org-trim
	  (org-export-data
	   (if (and toc headlinep)
	       (org-export-get-alt-title element info)
	     (org-element-property :title element))
	   info))))

    (format "%s %s" (make-string number ?#) text)))


(defun org-gemini-headline (headline contents info)
  "Transcode a HEADLINE element from Org to ASCII.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Don't export footnote section, which will be handled at the end
  ;; of the template.
  (unless (org-element-property :footnote-section-p headline)
    (let* ((low-level (org-export-low-level-p headline info))
	   (width (org-ascii--current-text-width headline info))
	   ;; Export title early so that any link in it can be
	   ;; exported and seen in `org-ascii--unique-links'.
	   (title (org-gemini--build-title headline info width (not low-level)))
	   ;; Blank lines between headline and its contents.
	   ;; `org-ascii-headline-spacing', when set, overwrites
	   ;; original buffer's spacing.
	   (pre-blanks
	    (make-string (or (car (plist-get info :ascii-headline-spacing))
			     (org-element-property :pre-blank headline)
			     0)
			 ?\n))
	   (links (and (plist-get info :ascii-links-to-notes)
		       (org-gemini--describe-links
			(org-ascii--unique-links headline info) width info)))
	   ;; Re-build contents, inserting section links at the right
	   ;; place.  The cost is low since build results are cached.
	   (body
	    (if (not (org-string-nw-p links)) contents
	      (let* ((contents (org-element-contents headline))
		     (section (let ((first (car contents)))
				(and (eq (org-element-type first) 'section)
				     first))))
		(concat (and section
			     (concat (org-element-normalize-string
				      (org-export-data section info))
				     "\n\n"))
			links
			(mapconcat (lambda (e) (org-export-data e info))
				   (if section (cdr contents) contents)
				   ""))))))
      ;; Deep subtree: export it as a list item.
      (if low-level
	  (let* ((bullets (cdr (assq (plist-get info :ascii-charset)
				     (plist-get info :ascii-bullets))))
		 (bullet
		  (format "%c "
			  (nth (mod (1- low-level) (length bullets)) bullets))))
	    (concat bullet title "\n" pre-blanks
		    ;; Contents, indented by length of bullet.
		    (org-ascii--indent-string body (length bullet))))
	;; Else: Standard headline.
	(concat title "\n" pre-blanks body)))))

(defun org-gemini-template (contents info)
  "Return complete document string after ASCII conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((global-margin (plist-get info :ascii-global-margin)))
    (concat
     ;; Build title block.
     (format "# %s\n" (org-export-data
		       (when (plist-get info :with-title) (plist-get info :title)) info))
     ;; Document's body.
     contents
     )))



(defun org-gemini-export-as-gemini (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'gemini "*Org Gemini Export*" async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

(defun org-gemini-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'gemini "*Org Gemini Export*" async subtreep visible-only body-only ext-plist (lambda () (text-mode))))


(defun org-gemini-export-to-file (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((file (org-export-output-file-name ".gmi" subtreep)))
    (org-export-to-file 'gemini file
      async subtreep visible-only body-only ext-plist)))


(provide 'ox-gemini)
