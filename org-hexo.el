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


;;;; Group

(defgroup org-hexo nil
  "Options for exporting Org mode files to hexo markdown."
  :tag "Org Export to hexo markdown files."
  :group 'org-export
  :link '(url-link :tag "Github" "https://github.com/coldnew/org-hexo"))


;;;; Custom

(defcustom org-hexo-project-alist nil
  "Project list for org-hexo, it can contains many project, all
  project must config like following:

(setq org-hexo-project-alist
      '((\"coldnew's blog\" :config \"~/Workspace/blog/config.el\")))
"
  :group 'org-hexo
  :type 'list)


;;;; Config Variables
;; NOTE: These variable should be defined in user's config.el file

(defvar org-hexo-output-directory nil
  "This variable should be defined in user's org-hexo config.el.
This path also contains org-mode's `~/.org-timestamp' file as cache.")


(defvar org-hexo-clear-ouput-when-republish nil
  "When t, clean files in `org-hexo-ouput-directory' when republish
project.")


(defvar org-hexo-overwrite-updated nil
  "Enable this to make org-hexo auto add `updated:' in markdown
file when not specify,the value will fetch from `date:'
front-matter.")

(defvar org-hexo-date-format "%Y-%02m-%02d %02H:%02M:%02S"
  "Format use in #+DATE: metadata.")

(defvar org-hexo-permalink-format ""
  "Format use in #+PERMALINK: metadata.")

(defvar org-hexo-newpost-template nil
  "The template file for create hexo newpost.")


;;;; Internal Variables

(defvar org-hexo-cache-filelist nil
  "List to storage where to remove the cache file. This variable
  will be rebuilf in `org-hexo--select-project'")


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

(defun org-hexo--clear-private-variables ()
  "Clear all private variables in org-hexo."
  (setq
   ;; Config variables
   org-hexo-output-directory nil
   org-hexo-clear-ouput-when-republish nil
   org-hexo-overwrite-updated nil
   org-hexo-date-format "%Y-%02m-%02d %02H:%02M:%02S"
   org-hexo-permalink-format ""
   org-hexo-newpost-template nil
   ;; Internal Variables
   org-hexo-cache-filelist nil))

(defun org-hexo--select-project (func &optional msg)
  "Prompt to select project to publish. If only one project
list in `org-hexo-project-alist', do not prompt."
  (let ((project
         (if (= (length org-hexo-project-alist) 1)
             (list (car org-hexo-project-alist))
           (list
            (assoc (org-icompleting-read
                    (or msg "Publish org-hexo project: ")
                    org-hexo-project-alist nil t)
                   org-hexo-project-alist)
            current-prefix-arg))))
    ;; clear all private variable before load config file.
    (org-hexo--clear-private-variables)
    ;; load config according org-hexo-project-list
    (let ((config (plist-get (cdar project) :config)))
      (if config (load config)
        (error (format "config %s not exist") config)))

    (let ((project-list (list org-hexo-publish-project-alist)))
      ;; rebuild cache filelist
      (setq org-hexo-cache-filelist nil)
      (dolist (c (car project-list))
        (add-to-list
         'org-hexo-cache-filelist
         (f-join org-hexo-output-directory (concat (car c) ".cache"))))

      (mapc
       (lambda (current-project)
         (funcall func current-project))

       (org-publish-expand-projects project-list)))))

(defun org-hexo--publish-project (project &optional force)
  "Publush all org-hexo post, if post already posted and not modified,
skip it.

When force is t, re-publish selected org-hexo project."
  (let ((org-publish-project-alist project)
        (org-publish-timestamp-directory
         (file-name-as-directory org-hexo-output-directory)))

    ;; when repiblish org-hexo project, we need to remove all already exist cache
    ;; file store in `org-hexo-cache-filelist'
    (when force
      ;; clear cache file
      (dolist (c org-hexo-cache-filelist)
        (if (file-exists-p c) (delete-file c)))
      ;; if option on, clean all files in `org-hexo-output-directory'.
      (when org-hexo-clear-ouput-when-republish
        (let ((target-dir
               (cond
                ;; if target is symlink, remove symlink dir and recreate it
                ((f-symlink? org-hexo-output-directory) (file-symlink-p org-hexo-output-directory))
                ;; delete directory and recreate it
                ((f-directory? org-hexo-output-directory) org-hexo-output-directory)
                ;; FIXME: recreate it ?
                (t (error "BUG: unknown remove org-hexo-output-directory methd.")))))
          ;; delete target-dir and recreate it
          (f-delete target-dir t)
          (f-mkdir target-dir)))

      ;; publish all current project
      (org-publish-all force))))


;;;; Load all hexo exporter functions
;;
;; ox-hexo-core.el -- core or common use functions
;; ox-hexo-md.el   -- Markdown exporter
;; ox-hexo-html.el -- HTML exporter
(mapcar (lambda (x) (require (intern (format "ox-hexo-%s" x)) nil t))
        '("core" "md" "html"))


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

;;;###autoload
(defun org-hexo-insert-template (&optional filename)
  "Insert org-hexo newpost template."
  (interactive)
  )


;;;###autoload
(defun org-hexo-publish (&optional force)
  "Published modified org-hexo files."
  (interactive)
  (org-hexo--select-project 'org-hexo--publish-project))

;;;###autoload
(defun oeg-hexo-republish (&optional force)
  "Re-publish all org-hexo files."
  (interactive)
  (noflet ((org-hexo--republish-project
            (project-list)
            (--publish-project project-list t)))
    (org-hexo--select-project 'org-hexo--republish-project)))

;;;###autoload
(defun org-hexo-publish-current-post (&optional force)
  "Published current post."
  (interactive)
  ;; Check if current post is belong to org-hexo.
  )

(provide 'org-hexo)
;;; org-hexo.el ends here.
