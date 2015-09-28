;;; org-hexo.el --- Export org-mode to hexo.

;; Copyright (c) 2015 Yen-Chin, Lee. (coldnew) <coldnew.tw@gmail.com>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://github.com/coldnew/org-hexo
;; Version: 0.1
;; Package-Requires: ((org "8.0") (cl-lib "0.5") (f "0.17.2") (blogit "0.1"))

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


;;;; Load all pelican exporter functions
;;
;; ox-pelican-core.el -- core or common use functions
;; ox-pelican-html.el -- HTML exporter
;; ox-pelican-md.el   -- Markdown exporter
;;
(mapcar (lambda (x) (require (intern (format "ox-hexo-%s" x)) nil t))
        '("core" "html" "md"))


;;;; End User Functions

;;;###autoload
(defun org-hexo-toggle-status ()
  "Toggle current org-mode file status as `draft' or `published'.
If #+STATUS: tag not exist, set current status as `draft'."
  (interactive)
  (let ((status (or (blogit-get-option :status) "published")))
    (if (string= status "draft")
        (blogit-set-option :status "published")
      (blogit-set-option :status "draft"))))

;;;###autoload
(defun org-hexo-update-date ()
  "Update #+DATE: tag with current date info."
  (interactive)
  (blogit-set-option :date
                     (format-time-string org-hexo-date-format)))



(provide 'org-hexo)
;;; org-hexo.el ends here.
