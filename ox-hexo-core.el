;;; ox-hexo-core.el --- Core functions for ox-hexo.el.

;; Copyright (c) 2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/org-pelican
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

(require 'noflet)
(require 'f)
(require 's)
(require 'ox-publish)


;;;; Backend general

;; pelican metadata
(defvar org-hexo--options-alist
  '(
    (:layout    "LAYOUT"     nil     nil)
    (:date      "DATE"       nil     nil)
    (:update    "UPDATE"     nil     nil)
    (:comments  "COMMENTS"   nil     nil)
    (:tags      "TAGS"       nil     nil)
    (:category  "CATEGORY"   nil     nil)
    (:permalink "PERMALINK"  nil     nil)
    ;; TODO:
    (:status   "STATUS"     nil     nil)
    ;; enable htmlize syntax highlight
    (:htmlize   "HTMLIZE"    nil     nil)
    ))


;;;; Internal functions

(defun org-hexo--protect-tag (tag)
  "Convert:
       _     ->  <space>
       @     ->  -
     <space> ->  ,
"
  (replace-regexp-in-string
   "_" " "
   (replace-regexp-in-string
    " " ","
    (replace-regexp-in-string
     "@" "-"
     tag))))

(defun org-hexo--protect-string (str)
  "Convert \" -> &quot;"
  (replace-regexp-in-string
   "\"" "&quot;" (org-html-encode-plain-text str)))

(defun org-hexo--protect-string* (str)
  (org-hexo--protect-tag
   (org-hexo--protect-string str)))

(defun org-hexo--do-copy (src dst &optional copyf args)
  "Copy SRC into DST. If `dired-do-sync' is found it would be
preferred. Otherwise, `copy-directory' or `copy-files' would be
used.

A copy function COPYF and its arguments ARGS could be specified."
  (let* ((dirp (file-directory-p src))
         (dst-dir (file-name-directory dst))
         (copyf (cond
                 (copyf copyf)
                 ((functionp 'dired-do-sync) 'dired-do-sync)
                 (dirp 'copy-directory)
                 (t 'copy-file)))
         (args (or args
                   (when (eq 'copy-file copyf) '(t t t)))))

    (unless (or (not dst-dir) (file-exists-p dst-dir)) (make-directory dst-dir t))

    (when (file-exists-p src)
      (apply copyf src dst args))))



;;;; Paragraph

(defun org-hexo--paragraph (func paragraph contents info)
  "Transcode PARAGRAPH element into Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let* (;; Fix multibyte language like chinese will be automatically add
         ;; some space since org-mode will transpose auto-fill-mode's space
         ;; to newline char.
         (fix-regexp "[[:multibyte:]]")
         (fix-contents
          (replace-regexp-in-string
           (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" contents))
         ;; Unfill paragraph to make contents look more better
         (unfill-contents
          (with-temp-buffer
            (insert fix-contents)
            (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil (point-min) (point-max))
            (buffer-string))))

    ;; Send modify data to func
    (funcall func paragraph unfill-contents info)))


;;;; Link

(defun org-hexo--link (func link contents info)
  "Transcode LINE-BREAK object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type link))
         (raw-link (org-element-property :path link))
         (raw-path (expand-file-name raw-link))
         (hexo-link (funcall func link contents info))
         (permalink (plist-get info :permalink))
         (output-file (plist-get info :output-file))
         ;; FIXME: make user control the data dir name
         (asset-dir (f-join (file-name-sans-extension output-file) "data")))

    ;; file
    (when (string= type "file")

      ;; check if file porint to absolute path
      (when (file-name-absolute-p raw-link)
        ;; calculate relative link for current post
        (setq raw-link (f-relative raw-path
                                   (file-name-directory (plist-get info :input-file))))

        (setq hexo-link (s-replace (concat "file://" raw-path) raw-link hexo-link)))

      ;; Create hexo's asset directory to save file
      (make-directory asset-dir t)

      ;; Copy file to asset dir
      (message (format "Copy files %s to %s." raw-path asset-dir))
      (org-hexo--do-copy raw-path  (f-slash asset-dir))

      ;; change link to use asset dir
      (setq hexo-link (s-replace raw-link
                                 (f-join "data" (file-name-nondirectory raw-path)) hexo-link))
      )

    hexo-link))


;;;; Metadata

(defun org-hexo--parse-date (info)
  "Parse #+DATE: value."
  (let ((date (plist-get info :date)))
    (and (org-string-nw-p date)
         (if (stringp date)
             ;; raw date info: 2013-08-04 23:28:44
             ;; FIXME: does this also work for `2013/08/04 23:28:44' ?
             date
           ;; parse org-timestamp
           (format-time-string "%Y-%m-%d %H:%M:%S"
                               (apply 'encode-time (org-parse-time-string
                                                    (org-element-property :raw-value date))))))))

(defun org-hexo--parse-title (info)
  "Parse #+TITLE: value."
  (let ((title (plist-get info :title)))
    (org-export-data (or title "") info)))

;; TODO: how about #+AUTHORS: ?
(defun org-hexo--parse-author (info)
  "Parse #+AUTOHR: value."
  (and (plist-get info :with-author)
       (let ((auth (plist-get info :author)))
         (and auth
              ;; Return raw Org syntax, skipping non
              ;; exportable objects.
              (org-element-interpret-data
               (org-element-map auth
                   (cons 'plain-text org-element-all-objects)
                 'identity info))))))

(defun org-hexo--parse-gravatar (info)
  "Generate metadata for gravatar from #+EMAIL:."
  (let ((email (plist-get info :email)))
    (if email
        (format "http://www.gravatar.com/avatar/%s" (md5 email))
      "")))

;; :date: 2010-10-03 10:20
;; :modified: 2010-10-04 18:40
;; :tags: thats, awesome
;; :category: yeah
;; :slug: my-super-post
;; :authors: Alexis Metaireau, Conan Doyle
;; :summary: Short version for index and feeds
;; :lang: en
;; :translation: true
;; :status: draft
;; :status: published
(defun org-hexo--build-meta-info
    (info title-format metainfo metainfo* toc)
  "Return meta tags for exported document.
INFO is a plist used as a communication channel.
"
  (let ((author (org-hexo--parse-author info))
        (title (org-hexo--parse-title info))
        (date (org-hexo--parse-date info))
        (description (plist-get info :description))
        (keywords (plist-get info :keywords))
        (category (plist-get info :category))
        (tags (plist-get info :tags))
        (permalink (plist-get info :permalink))
        (lang (plist-get info :language))
        (status (plist-get info :status))) ;; NOTE: value: draft, published
    (concat

     (format title-format title)
     "\n"

     (funcall metainfo "author" author)
     (funcall metainfo "date" date)

     (funcall metainfo "lang" lang)
     (funcall metainfo "description" description)
     (funcall metainfo "keywords" keywords)

     (funcall metainfo "permalink" permalink)
     (funcall metainfo "status" status)

     ;; compact version
     (funcall metainfo* "category" category)
     (funcall metainfo* "tags" tags)

     ;; Table of contents
     ;; (let ((depth (plist-get info :with-toc)))
     ;;   (when depth
     ;;     (funcall metainfo "toc" (funcall toc depth info))))
     ;; end of yaml
     "\n---\n"
     ;; Add generator comments
     ;;"<!-- This file is generate by org-hexo, DO NOT EDIT manually -->\n"
     )))

;; buffer   (plist-get info :input-buffer)
;; filename (plist-get info :input-file)
;; output-file (plist-get info :output-file)

;; (:export-options
;;  nil
;;  :input-buffer "編譯 wandboard 的 Android 4.4.2 系統.org<tmp>"
;;  :input-file "/Users/coldnew/Workspace/hexo-blogs/tmp/blog/編譯 wandboard 的 Android 4.4.2 系統.org"
;;  :layout nil
;;  :date "2015-09-07 23:28:44")
;; -

(provide 'ox-hexo-core)
;;; ox-hexo-core.el ends here.
