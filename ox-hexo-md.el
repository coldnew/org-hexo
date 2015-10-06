;;; ox-hexo-md.el --- Export org-mode to hexo markdown.

;; Copyright (c) 2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/org-hexo
;; Version: 0.1
;; Package-Requires: ((org "8.0") (cl-lib "0.5") (f "0.17.2") (noflet "0.0.11"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'f)
(require 'ox-html)
(require 'ox-md)
(require 'ox-publish)

(require 'ox-hexo-core)


;;;; Backend

(org-export-define-derived-backend 'hexo-md 'md
  :translate-alist
  '(
    ;; Fix for multibyte language
    (paragraph . org-hexo-md-paragraph)
    ;; Fix for pelican metadata
    (template . org-hexo-md-template)
    ;; Fix link path to suite for pelican
    (link . org-hexo-md-link)
    ;; Make compatible with pelican
    (src-block . org-hexo-md-src-block)
    (example-block . org-hexo-md-example-block)
    (quote-block . org-hexo-md-quote-block)
    ;; Increase headline level
    (headline . org-hexo-md-headline)
    (inner-template . org-hexo-md-inner-template)
    (table . org-hexo-md-table)
    )
  :options-alist org-hexo--options-alist)


;;;; Paragraph

(defun org-hexo-md-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  ;; Send modify data to org-md-paragraph
  (org-hexo--paragraph 'org-md-paragraph paragraph contents info))


;;; Template

(defun org-hexo-md-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)))


;;;; Link

(defun org-hexo-md-link (link contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((org-md-link-org-files-as-md nil)
        (md-link (org-hexo--link 'org-md-link link contents info)))
    ;; Fancybox support
    ;; convert:
    ;;    ![img]({filename}data/test.png)   ->  ![img]({filename}data/test.png){.fancybox}
    ;; (replace-regexp-in-string
    ;;  "!\\[img\\](\\(.*?\\))" "![img](\\1){.fancybox}" md-link)

    ;; convert:
    ;;    ![img](data/test.png)   -> ![](data/test.png)
    (replace-regexp-in-string
     "!\\[img\\](\\(.*?\\))" "![](\\1)" md-link)
    ))


;;;; Table

(defun org-hexo-md-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  ;;  (org-html-encode-plain-text
  ;; remove newline
  ;; NOTE: https://github.com/iissnan/hexo-theme-next/issues/114
  (replace-regexp-in-string "\n" ""
                            (org-html-table table contents info)))
;;)


;;;; Headline

(defun org-hexo-md-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (let* ((info (plist-put info :headline-offset 1)))
    (org-md-headline headline contents info)))


;;;; Example Block and Src Block

;;;; Example Block

(defun org-hexo-md-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((htmlize (plist-get info :hexo-htmlize)))
    (if htmlize
        (org-html-example-block example-block contents info)
      ;; convert example block to markdown syntax
      (replace-regexp-in-string
       "^" "    "
       (org-remove-indentation
        (org-export-format-code-default example-block info)))
      )))

;;;; Src Block

(defun org-hexo-md-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let ((lang (org-element-property :language src-block))
        (htmlize (plist-get info :hexo-htmlize)))

    (if htmlize
        ;; FIXME:
        ;;  we remove newline here since hexo's parser
        ;;  will generate extra newline
        (replace-regexp-in-string
         "</pre>\n</div>" "</pre></div>"
         (replace-regexp-in-string
          "<div class=\"org-src-container\">\n\n<pre" "<div class=\"org-src-container\"><pre"
          ;; when use in markdown, we need to convert `\' to `\\'
          (replace-regexp-in-string
           "\\\\" "\\\\\\\\"
           (format "%s"
                   (org-html-src-block src-block contents info)))))

      ;; Convert to hexo markdown format
      (concat
       (format "{%% codeblock lang:%s %%}" lang)
       "\n"
       (format "%s"
               (org-md-example-block src-block contents info))
       (format "{%% endcodeblock %%}"))
      )))


;;;; Quote Block

(defun org-hexo-md-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "{% centerquote %}%s{% endcenterquote %}" contents))


;;;; Template

(defun org-hexo-md---build-meta-info (name var func)
  (and (org-string-nw-p var)
       (format "%s: %s\n" name
               (concat
                (if (s-equals-p name "tags") "[ " "")
                (funcall func var)
                (if (s-equals-p name "tags") " ]" "")))
       ))

(defun org-hexo-md--build-meta-info (info)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel."
  (concat
   "---\n"
   (org-hexo--build-meta-info
    info
    ;; title format
    "title: %s"
    ;; method to build generic metainfo
    '(lambda (name var)
       (org-hexo-md---build-meta-info name var 'org-hexo--protect-string))
    ;; method to build compact metainfo
    '(lambda (name var)
       (org-hexo-md---build-meta-info name var 'org-hexo--protect-string*))
    )
   "---\n"
   ;; Add generator comments
   "<!-- This file is generate by org-hexo, DO NOT EDIT manually -->\n"))

(defun org-hexo-md-template (contents info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (concat
   (org-hexo-md--build-meta-info info)
   "\n"
   contents))


;;; End-user functions

;;;###autoload
(defun org-hexo-export-as-markdown
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for blogit.

Export is done in a buffer named \"*Blogit HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'hexo-md "*hexo markdown Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (markdown-mode))))

;;;###autoload
(defun org-hexo-publish-to-markdown (plist filename pub-dir)
  "Publish an org file to rst.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'hexo-md filename ".md"
                      plist pub-dir))

(provide 'ox-hexo-md)
;;; ox-hexo-md.el ends here.
