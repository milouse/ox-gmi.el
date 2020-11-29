;;; ox-gemini.el --- Gemini Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2020 Free Software Foundation, Inc.

;; Author: Étienne Deparis <etienne@depar.is>
;; Keywords: org, gemini, gmi

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Gemini back-end for Org exporter, based on
;; `markdown' back-end.  See Org manual for more information.

;;; Code:

(require 'ox-md)
(require 'ox-ascii)
(require 'ox-publish)


;;; Define Back-End

(org-export-define-derived-backend 'gmi 'md
  :menu-entry
  '(?g "Export to Gemini"
       ((?G "To temporary buffer"
	    (lambda (a s v b) (org-gmi-export-as-gemini a s v)))
	(?g "To file" (lambda (a s v b) (org-gmi-export-to-gemini a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-gmi-export-to-gemini t s v)
		(org-open-file (org-gmi-export-to-gemini nil s v)))))))
  :translate-alist '((center-block . org-gmi--convert-to-ascii)
		             (example-block . org-gmi-preformatted-block)
		             (export-block . org-gmi-export-block)
		             (fixed-width . org-gmi-preformatted-block)
		             (headline . org-gmi-headline)
                     (inlinetask . org-gmi--convert-to-ascii)
		             (inner-template . org-gmi-inner-template)
		             (item . org-gmi-item)
		             (keyword . org-gmi-keyword)
		             (line-break . org-gmi-line-break)
		             (link . org-gmi-link)
		             (paragraph . org-gmi-paragraph)
		             (property-drawer . org-gmi-preformatted-block)
		             (quote-block . org-gmi-quote-block)
                     (section . org-gmi-section)
                     (special-block . org-gmi--convert-to-ascii)
		             (src-block . org-gmi-preformatted-block)
		             (table . org-gmi--convert-to-ascii)))


;;; Inner Variables

(defvar org-gmi--links-in-section '()
  "AList storing all links in current section")



;;; Inner Functions

(defun org-gmi--convert-to-ascii (datum _contents info)
  "Convert DATUM into ASCII, including contents."
  (let ((org-ascii-charset 'utf-8)
        (org-ascii-links-to-notes t)
        (org-ascii-text-width 80))
    (org-export-data-with-backend datum 'ascii info)))

(defun org-gmi--format-paragraph (content &optional prefix)
  "Transcode PARAGRAPH element into Gemini format.
If PREFIX is non-nil, add it at the beginning of each lines."
  (replace-regexp-in-string
   "^\\s-?" (or prefix "")
   (org-trim
    (replace-regexp-in-string "\r?\n\\([^\r\n]\\)" " \\1" content))))

(defun org-gmi--describe-links (links)
  "Return a string describing a list of links.
LINKS is an alist like `org-gmi--links-in-section'"
  (mapconcat
   #'(lambda (link)
       (let ((dest (car link))
             (reference (cadr link))
             (label (car (cddr link))))
         (format "=> %s [%s] %s" dest reference label)))
   links "\n"))


;;; Transcode Functions

(defun org-gmi-preformatted-block (block _contents info)
  "Transcode BLOCK element into Gemini format.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (let ((language (org-export-data (org-element-property :language block) info))
        (caption (org-export-data (org-element-property :caption block) info)))
    (setq caption (if caption (format "%s (%s)" language caption) language))
    (format "```%s\n%s```\n"
            caption
            (org-remove-indentation
             (org-export-format-code-default block info)))))

(defun org-gmi-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Gemini.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (member (org-element-property :type export-block) '("GEMINI" "GMI"))
      (org-remove-indentation (org-element-property :value export-block))
    ;; Also include HTML export blocks.
    (org-export-with-backend 'html export-block contents info)))

(defun org-gmi-headline (headline contents info)
  "Transcode HEADLINE element into Gemini format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (let* ((level (org-export-get-relative-level headline info))
	     (title (org-export-data (org-element-property :title headline) info))
	     (todo (and (plist-get info :with-todo-keywords)
		            (let ((todo (org-element-property :todo-keyword
							                          headline)))
			          (and todo (concat (org-export-data todo info) " ")))))
	     (tags (and (plist-get info :with-tags)
		            (let ((tag-list (org-export-get-tags headline info)))
			          (and tag-list
			               (concat "     " (org-make-tag-string tag-list))))))
	     (priority
	      (and (plist-get info :with-priority)
		       (let ((char (org-element-property :priority headline)))
		         (and char (format "[#%c] " char)))))
	     ;; Headline text without tags.
	     (heading (concat todo priority title)))
    (if
        ;; Cannot create a headline.  Fall-back to a list.
        (or (org-export-low-level-p headline info)
	        (> level 6))
	    (let ((bullet
	           (if (not (org-export-numbered-headline-p headline info)) "*"
		         (concat (number-to-string
			              (car (last (org-export-get-headline-number
				                      headline info))))
			             "."))))
	      (concat bullet (make-string (- 4 (length bullet)) ?\s)
                  heading tags "\n\n" contents))
      ;; Else
	  (concat (org-md--headline-title 'atx level heading nil tags)
		      contents))))

(defun org-gmi-inner-template (contents info)
  "Return body of document after converting it to Gemini syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  ;; Make sure CONTENTS is separated from table of contents and
  ;; footnotes with at least a blank line.
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth
       (concat (org-gmi--build-toc info (and (wholenump depth) depth)) "\n")))
   ;; Document contents.
   contents
   "\n"
   ;; Footnotes section.
   (org-md--footnote-section info)))

(defun org-gmi-item (item contents info)
  "Transcode ITEM element into Gemini format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (replace-regexp-in-string
   "^-" "*" (org-md-item item contents info)))

(defun org-gmi-keyword (keyword contents info)
  "Transcode a KEYWORD element into Gemini format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (member (org-element-property :key keyword) '("GEMINI" "GMI"))
    (org-element-property :value keyword)))

(defun org-gmi-line-break (_line-break _contents _info)
  "Transcode LINE-BREAK object into Gemini format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  " ")

(defun org-gmi-link (link desc info)
  "Transcode a LINK object from Org to Gemini.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let* ((href (org-element-property :raw-link link))
         (label (or desc href))
         (link-data (assoc href org-gmi--links-in-section))
         ;; Default next-reference
         (next-reference (1+ (length org-gmi--links-in-section))))
    ;; As links are specific for a section, which should not be that long (?),
    ;; we will always use the first label encountered for a link as reference.
    (unless link-data
      (setq link-data `(,href ,next-reference ,label))
      (add-to-list 'org-gmi--links-in-section link-data t))
    (format "%s[%s]" label (cadr link-data))))

(defun org-gmi-paragraph (_paragraph contents _info)
  "Transcode PARAGRAPH element into Gemini format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (org-gmi--format-paragraph contents))

(defun org-gmi-quote-block (_quote-block contents _info)
  "Transcode QUOTE-BLOCK element into Gemini format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (org-gmi--format-paragraph contents "> "))

(defun org-gmi-section (section contents info)
  "Transcode SECTION into Gemini format."
  (let ((output
         (concat contents "\n"
                 (org-gmi--describe-links org-gmi--links-in-section))))
    ;; Reset link list
    (setq org-gmi--links-in-section '())
    output))


;;; Interactive function

;;;###autoload
(defun org-gmi-export-as-gemini (&optional async subtreep visible-only)
  "Export current buffer to a Gemini buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Gemini Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer
   'gmi "*Org Gemini Export*"
   async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-gmi-export-to-gemini (&optional async subtreep visible-only)
  "Export current buffer to a Gemini file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".gmi" subtreep)))
    (org-export-to-file 'gmi outfile async subtreep visible-only)))


;;;###autoload
(defun org-gmi-publish-to-gemini (plist filename pub-dir)
  "Publish an org file to Gemini.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'gmi filename ".gmi" plist pub-dir))

(provide 'ox-gmi)

;;; ox-gmi.el ends here