;;; ox-gemini.el --- Output gemini formatted documents from org-mode  -*- lexical-binding: t; -*-

;; Author: Justin Abrahms <justin@abrah.ms>
;; URL: https://git.sr.ht/~abrahms/ox-gemini
;; Keywords: lisp gemini
;; Version: 0
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later


;;; Commentary:
;;
;; There's a web-alternative that's similar to the gopher protocol
;; named 'gemini'.  You can find more about it at
;; https://gemini.circumlunar.space/ This package serves as an
;; org-mode export backend in order to build those types of
;; document-oriented sites.

(require 'ox)
(require 'ox-publish)
(require 'ox-ascii)
(require 'cl-lib)
(require 'url-util)

;; TODO:
;; Sublists aren't supported in gemini
;; There's a trailing space after inline code samples
;; If you link a file to an absolute path, the links break
;; bare links don't work (e.g. directly linking https://google.com)

;;; Code:

(org-export-define-derived-backend 'gemini 'ascii
  :menu-entry
  '(?g "Export to Gemini"
       ((?b "To buffer"
            (lambda (a s v b)
              (org-gemini-export-to-buffer a s v b nil)))
        (?f "To file"
            (lambda (a s v b)
              (org-gemini-export-to-file a s v b nil)))))
  :translate-alist '((quote-block . org-gemini-quote-block)
                     (code . org-gemini-code-inline)
                     (export-block . org-gemini-export-block)
                     (paragraph . org-gemini-paragraph)
                     (headline . org-gemini-headline)
                     (link . org-gemini-link)
                     (section . org-gemini-section)
                     (src-block . org-gemini-code-block)
                     (item . org-gemini-item)
                     (template . org-gemini-template)
                     (table . org-gemini-table)))

(defun org-gemini-paragraph (_paragraph contents _info)
  "CONTENTS is the text of the paragraph."
  (concat (replace-regexp-in-string "\n" " " contents)
          "\n"))

(defun org-gemini-item (item contents info)
  "Generate a Gemtext item from the org CONTENTS.
CONTENTS is the text of the individual item.
ITEM is the parsed-org element with all properties."
  (concat "* "
          ;; vv Code from ox-md! vv
	  (pcase (org-element-property :checkbox item)
	    (`on "[X] ")
	    (`trans "[-] ")
	    (`off "[ ] "))
	  (let ((tag (org-element-property :tag item)))
	    (and tag (format "%s :: " (org-export-data tag info))))
          ;; ^^ ^^
          contents))

(defun org-gemini-quote-block (_input contents _info)
  "CONTENTS is the text of the quote."
  (replace-regexp-in-string
   "^" "> "
   (replace-regexp-in-string "\n\\'" "" contents)))

(defun org-gemini-code-inline (input _contents info)
  "Generate an inline code in Gemtext from the parsed INPUT.
INPUT is either a '\\=src-block\\=' or '\\=example-block\\='
element.  INFO is a plist."
  ;; there's a bug here where there's a trailing space in the ``
  (format "`%s`" (org-export-format-code-default input info)))

(defun org-gemini-code-block (src-block _contents info)
  "SRC-BLOCK is a codeblock.  INFO is a plist."
  (let ((name (or (caaar (org-element-property :caption src-block))
                  (org-element-property :name src-block))))
    (org-remove-indentation
     (format "```%s\n%s```"
             (or name "")
             (org-export-format-code-default src-block info)))))

(defun org-gemini--describe-links (links _width info)
  "Describe links is the footer-portion of the link data.

It's output just before each section.  LINKS is a list of each link.
INFO is a plist."
  (concat
   (mapconcat
    (lambda (link)
      (let* ((raw-path (org-element-property :raw-link link))
             (link-type (org-element-property :type link))
             (is-org-file-link (and (string= "file" link-type)
                                    (string= ".org" (downcase (file-name-extension raw-path ".")))))
             (path (if is-org-file-link
                       (concat (file-name-sans-extension (org-element-property :path link)) ".gmi")
                     raw-path))
             (desc (org-element-contents link))
             (anchor (string-replace "\n" " "
                                     (org-export-data
                                      (or desc (org-element-property :raw-link link))
                                      info))))
        (format "=> %s %s\n" (url-encode-url path) anchor)))
    links "")
   (when (car links)
     "\n")))


(defun org-gemini-link (_link desc _info)
  "Simple link generation.

DESC is the link text

Note: the footer with the actual links are handled in
`org-gemini--describe-links'."
  (if (org-string-nw-p desc)
      (format "[%s]" desc)))


(defun org-gemini-section (section contents info)
  "Transcode a SECTION element from Org to GEMINI.
CONTENTS is the contents of the section.  INFO is a plist holding
contextual information."
  ;; CONTENTS is nil when the section has no text but it has properties setted.
  (let ((contents-str (or contents "")) ;; ensure that contents is a string.
        (links
         (and (plist-get info :ascii-links-to-notes)
              ;; Take care of links in first section of the document.
              (not (org-element-lineage section '(headline)))
              (org-gemini--describe-links
               (org-ascii--unique-links section info)
               (org-ascii--current-text-width section info)
               info))))
    (org-remove-indentation
      (if (not (org-string-nw-p links)) contents-str
        (concat (org-element-normalize-string contents-str) "\n\n" links))
      ;; Do not apply inner margin if parent headline is low level.
      (let ((headline (org-export-get-parent-headline section)))
        (if (or (not headline) (org-export-low-level-p headline info)) 0
          (plist-get info :ascii-inner-margin))))))

(defun org-gemini--build-title
    (element info _text-width &optional _underline _notags toc)
    "Build a title heading.

ELEMENT is an org-element.  TOC is whether to show the table of contents.
INFO is unimportant."
  (let ((number (org-element-property :level element))
        (text
         (org-trim
          (org-export-data
           (if toc
               (org-export-get-alt-title element info)
             (org-element-property :title element))
           info))))

    (format "%s %s" (make-string number ?#) text)))


(defun org-gemini-headline (headline contents info)
  "Transcode a HEADLINE element from Org to GEMINI.
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
                    ;; In Gemtext, text should not be indentend. Otherwise,
                    ;; source code blocks, links, and other line types would not
                    ;; be interpreted by clients because of the initial spacing.
                    body))
        ;; Else: Standard headline.
        (concat title "\n" pre-blanks body)))))

(defun org-gemini-template (contents info)
  "Return complete document string after GEMINI conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (when (plist-get info :with-title)
                                  (plist-get info :title))
                                info)))
    (concat
     (unless (string= title "")
       (format "# %s\n\n" title))
     contents)))

(defun org-gemini-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  "Export an org file to a new buffer.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and
table of contents from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (org-export-to-buffer 'gemini "*Org Gemini Export*" async subtreep visible-only body-only ext-plist (lambda () (text-mode))))


(defun org-gemini-export-to-file (&optional async subtreep visible-only body-only ext-plist)
  "Export an org file to a gemini file.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and
table of contents from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((file (org-export-output-file-name ".gmi" subtreep)))
    (org-export-to-file 'gemini file
      async subtreep visible-only body-only ext-plist)))


(defun org-gemini-publish-to-gemini (plist filename pub-dir)
  "Publish an org file to a gemini file.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to
   'gemini filename ".gmi" plist pub-dir))

(defun org-gemini-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (member (org-element-property :type export-block)
                '("GEMINI" "GMI" "GEMTEXT"))
    (org-remove-indentation (org-element-property :value export-block))))

(defun org-gemini-table (table contents info)
  "Generate a Gemtext table from the parsed Org.
Use the `org-ascii-table' but surrounded by backticks.
Parameters: TABLE is the parsed org-element table.  CONTENTS is the text with
properties.  INFO is a plist with export options."
  (let ((name (or (caaar (org-element-property :caption table))
                  (org-element-property :name table))))
    (format "```%s\n%s\n```\n"
            (or name "")
            (org-ascii-table table contents info))))


(provide 'ox-gemini)
;;; ox-gemini.el ends here
