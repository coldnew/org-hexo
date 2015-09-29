;;; org-hexo.el --- Export org-mode to hexo.

;; Copyright (c) 2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/org-hexo
;; Version: 0.1
;; Package-Requires: ((org "8.0") (cl-lib "0.5") (f "0.17.2"))

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

(require 'blogit)


;;;; Group

(defgroup org-hexo nil
  "Options for exporting Org mode files to pelican."
  :tag "Org Export to hexo html/md files."
  :group 'org-export
  :link '(url-link :tag "Github" "https://github.com/coldnew/org-pelican"))


;;;; Custom

(defcustom org-hexo-date-format "%Y-%02m-%02d %02H:%02M:%02S"
  "Format use in #+DATE: metadata."
  :group 'org-hexo
  :type 'string)

(defcustom org-hexo-htmlize-src-block nil
  "Enable to use htmlize to render src block.
This one is global variable, if you want to make some page use htmlize, you
can specify `#+HTMLIZE: true' in your file"
  :group 'org-hexo
  :type 'boolean)

(defcustom org-hexo-overwrite-updated nil
  "Enable this to make org-hexo auto add `updated:' in markdown
file when not specify,the value will fetch from `date:'
front-matter."
  :group 'org-hexo
  :type 'boolean)

(defcustom org-hexo-newpost-template nil
  "The template file for create hexo newpost."
  :group 'org-hexo
  :type 'string)


;;; Internal helper functions

(defun org-hexo--string-to-key (string)
  "Conver string to key. eq: \"test\" -> :test"
  (intern (format ":%s" string)))

(defun org-hexo--symbol-to-key (symbol)
  "Conver symbol to key. eq: test -> :test"
  (org-hexo--string-to-key (symbol-name symbol)))

(defun org-hexo--key-to-string (key)
  "Conver key to string. eq: :test -> \"test\""
  (let ((key-str (symbol-name key)))
    (s-right (- (length key-str) 1) key-str)))

(defun org-hexo--key-to-symbol (key)
  "Conver key to symbol. eq: test -> :test"
  (intern (org-hexo--key-to-string key)))

(defun org-hexo--set-option (key value)
  "Modify option value of org file opened in current buffer.
If option does not exist, create it automatically."
  (let* ((option (upcase (org-hexo--key-to-string key)))
         (match-regexp (org-make-options-regexp `(,option)))
         (blank-regexp "^#\\+\\(\\w*\\):[        ]*\\(.*\\)")
         (insert-option '(insert (concat "#+" option ": " value)))
         (mpoint))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward match-regexp nil t)
          (progn
            (goto-char (point-at-bol))
            (kill-line)
            (eval insert-option))
        ;; no option found, insert it
        (progn
          (goto-char (point-min))
          (while (re-search-forward blank-regexp nil t)
            (setq mpoint (point)))
          (if (not mpoint) (setq mpoint (point-min)))
          (goto-char mpoint)
          (when (not (= mpoint (point-min)))
            (goto-char (point-at-eol))
            (newline-and-indent))
          (eval insert-option)
          (if (= mpoint (point-min))
              (newline-and-indent))
          )))))

(defun org-hexo--get-option (key)
  "Read option value of org file opened in current buffer.

This function will first use the standard way to parse org-option.
If parsing failed, use regexp to get the options, else return nil.
"
  (let* ((option (upcase (org-hexo--key-to-string key)))
         (match-regexp (org-make-options-regexp `(,option))))

    ;; use regexp to find options
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward match-regexp nil t)
        (match-string-no-properties 2 nil)))))


;;;; Load all hexo exporter functions
;;
;; ox-pelican-core.el -- core or common use functions
;; ox-pelican-md.el   -- Markdown exporter
;; FIXME: combine core and md
(mapcar (lambda (x) (require (intern (format "ox-hexo-%s" x)) nil t))
        '("core" "md"))


;;;; End User Functions

;; ;;;###autoload
;; (defun org-hexo-toggle-status ()
;;   "Toggle current org-mode file status as `draft' or `published'.
;; If #+STATUS: tag not exist, set current status as `draft'."
;;   (interactive)
;;   (let ((status (or (org-hexo--get-option :status) "published")))
;;     (if (string= status "draft")
;;         (org-hexo--set-option :status "published")
;;       (org-hexo--set-option :status "draft"))))

;;;###autoload
(defun org-hexo-update-date ()
  "Update #+DATE: tag with current date info."
  (interactive)
  (org-hexo--set-option :date
                     (format-time-string org-hexo-date-format)))

;;;###autoload
(defun org-hexo-update-modified ()
  "Update #+UPDATED: tag with current date info."
  (interactive)
  (org-hexo--set-option :updated
                     (format-time-string org-hexo-date-format)))

;; FIXME:
;;;###autoload
(defun org-hexo-new-post ()
  "Update #+DATE: tag with current date info."
  (interactive)
  )

;;;; TODO: remove blogit depends

(provide 'org-hexo)
;;; org-hexo.el ends here.
