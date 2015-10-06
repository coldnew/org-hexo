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



;;;; Customize Options

;; FIXME: this only use by myself
(defcustom org-hexo-enable-feed t
  ""
  :group 'org-hexo
  :type 'boolean)


;;;; Backend general


;; hexo metadata
(defvar org-hexo--options-alist
  '(;; buildin in org-mode
    (:date      "DATE"       nil     nil)
    (:tags      "TAGS"       nil     nil)
    (:category  "CATEGORY"   nil     nil)
    ;; Need by hexo
    (:layout    "LAYOUT"     nil     nil)
    (:updated   "UPDATED"    nil     nil)
    (:comments  "COMMENTS"   nil     nil)
    (:permalink "PERMALINK"  nil     nil)
    ;; TODO:
    (:status   "STATUS"     nil     nil)
    ;; #+HEXO: feed:t
    (:hexo-feed      nil    "feed"        org-hexo-enable-feed t)
    ))


;;;; Internal functions

(defun org-hexo--protect-tag (text)
  "Convert:
       _     ->  <space>
       @     ->  -
     <space> ->  ,
"
  (let ((protect-char-alist
         '(("@" . "-")
           (" " . ",")
           ("_" . " "))))
    (dolist (pair protect-char-alist text)
      (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))))

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
         (output-file (plist-get info :output-file)))

    ;; file
    (when (string= type "file")

      ;; check if file porint to absolute path
      (when (file-name-absolute-p raw-link)
        ;; calculate relative link for current post
        (setq raw-link (f-relative raw-path
                                   (file-name-directory (plist-get info :input-file))))

        (setq hexo-link (s-replace (concat "file://" raw-path) raw-link hexo-link)))

      (when output-file
        (let ;; FIXME: make user control the data dir name
            ((asset-dir (f-join (file-name-sans-extension output-file) "data")))
          ;; Create hexo's asset directory to save file
          (make-directory asset-dir t)

          ;; Copy file to asset dir
          (message (format "Copy files %s to %s." raw-path asset-dir))
          (org-hexo--do-copy raw-path  (f-slash asset-dir))

          ;; change link to use asset dir
          (setq hexo-link (s-replace raw-link
                                     (f-join "data" (file-name-nondirectory raw-path)) hexo-link)))))

    hexo-link))


;;;; Metadata

(defun org-hexo--parse-date (info key)
  "Parse #+DATE: value."
  (let ((date (plist-get info key)))
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

(defun org-hexo---build-front-matter (name var)
  (and (org-string-nw-p var)
       (format "%s: %s\n" name (org-hexo--protect-string var))))

(defun org-hexo---build-front-matter* (name var)
  (and (org-string-nw-p var)
       (format "%s: %s\n" name
               (concat
                (if (s-equals-p name "tags") "[ " "")
                (org-hexo--protect-string* var)
                (if (s-equals-p name "tags") " ]" "")))))

(defun org-hexo--build-front-matter (info)
  (let* ((date (org-hexo--parse-date info :date))
         (updated (or (org-hexo--parse-date info :updated) date))
         (category (plist-get info :category))
         (tags (plist-get info :tags))
         )
    (concat
     "---\n"
     ;; user info
     (org-hexo---build-front-matter "title" (org-hexo--parse-title info))
     (org-hexo---build-front-matter "author" (org-hexo--parse-author info))

     ;; date
     (org-hexo---build-front-matter "date" date)

     (when org-hexo-overwrite-updated
       (org-hexo---build-front-matter
        "updated"
        (or (org-hexo--parse-date info :updated) date)))

     (org-hexo---build-front-matter "lang" (plist-get info :language))
     (org-hexo---build-front-matter "description" (plist-get info :description))
     (org-hexo---build-front-matter "keywords" (plist-get info :keywords))
     (org-hexo---build-front-matter "permalink" (plist-get info :permalink))
     (org-hexo---build-front-matter "feed"  (plist-get info :hexo-feed))

     ;; NOTE: value: draft, published
     ;; FIXME: finish this function
     (org-hexo---build-front-matter "status" (plist-get info :status))

     ;; compact version
     (org-hexo---build-front-matter* "category" category)
     (org-hexo---build-front-matter* "tags" tags)
     "---\n"
     ;; Add generator comments
     "<!-- This file is generate by org-hexo, DO NOT EDIT manually -->\n")))

(provide 'ox-hexo-core)
;;; ox-hexo-core.el ends here.
