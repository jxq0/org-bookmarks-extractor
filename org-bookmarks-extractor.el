;;; org-bookmarks-extractor.el --- Extract bookmarks from Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Xuqing Jia

;; Author: Xuqing Jia <jxq@jxq.me>
;; URL: https://github.com/jxq0/org-bookmarks-extractor
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, org

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'org-element)

(defgroup org-bookmarks-extractor nil
  "Extract bookmarks from Org mode."
  :group 'org)

(defcustom org-bookmarks-extractor-html-file nil
  "Html file path."
  :type 'string)

(cl-defstruct (org-bookmarks-extractor-url
               (:constructor org-bookmarks-extractor-url-create)
               (:copier nil))
  title url)

(defun org-bookmarks-extractor-parse (org-file)
  "Parse ORG-FILE into org-data."
  (with-temp-buffer
    (insert-file-contents org-file)
    (org-element-parse-buffer)))

(defun org-bookmarks-extractor--prepend-nil (data orig-result)
  "Prepend nil into ORIG-RESULT if DATA only has headline children."
  (let* ((first-child (org-element-type (car (org-element-contents data)))))
    (if (eq first-child 'headline)
        (cons nil orig-result)
      orig-result)))

(defun org-bookmarks-extractor--walk (data)
  "Walk DATA and return a list like (TITLE (LINKS CHILD1 CHILD2 ...))."
  (let ((data-type (org-element-type data))
        (contents (org-element-contents data)))
    (pcase data-type
      ('headline
       (list (org-element-property :raw-value data)
             (org-bookmarks-extractor--prepend-nil
              data
              (mapcar #'org-bookmarks-extractor--walk contents))))

      ('org-data
       (list "root"
             (org-bookmarks-extractor--prepend-nil
              data
              (mapcar #'org-bookmarks-extractor--walk contents))))

      ('link (let* ((raw-link (org-element-property :raw-link data))
                    (title (substring-no-properties
                            (org-element-interpret-data contents))))
               (list (org-bookmarks-extractor-url-create :title title :url raw-link))))

      (_ (mapcan #'org-bookmarks-extractor--walk contents)))))

(defun org-bookmarks-extractor--to-html (data level)
  "Convert DATA returned by `org-bookmarks-extractor--walk' into html."
  (let* ((raw-title (car data))
         (indent (make-string (* level 4) 32))
         (links-indent (make-string (* (+ 1 level) 4) 32))
         (timestamp (format-time-string "%s"))
         (title (if (string= raw-title "root")
                    (format "\n%s<DT><H3 PERSONAL_TOOLBAR_FOLDER=\"true\">Bookmark Toolbar</H3>" indent)
                    (format "\n%s<DT><H3>%s</H3>" indent raw-title)))
         (links-data (car (nth 1 data)))
         (links (if links-data
                    (mapconcat
                     (lambda (x)
                       (format "\n%s<DT><A HREF=\"%s\" ADD_DATE=\"%s\">%s</A>"
                               links-indent
                               (org-bookmarks-extractor-url-url x)
                               timestamp
                               (org-bookmarks-extractor-url-title x)))
                     links-data "")
                  ""))
         (children-data (cdr (nth 1 data)))
         (children (mapconcat
                    (lambda (x)
                      (org-bookmarks-extractor--to-html x (+ 1 level)))
                    children-data ""))
         (result (format "%s\n%s<DL><p>%s%s\n%s</DL><p>" title indent links children indent)))
    result))

(defun org-bookmarks-extractor--to-html-wrapper (data)
  (let* ((raw-result (org-bookmarks-extractor--to-html data 1)))
    (format "<!DOCTYPE netscape-bookmark-file-1>\n<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">\n<TITLE>Bookmarks</TITLE>\n<H1>Bookmarks</H1>\n<DL><p>%s\n</DL><p>" raw-result)))

(defun org-bookmarks-extractor--extract (org-file html-file)
  "Extract bookmarks from ORG-FILE into HTML-FILE."
  (with-temp-buffer
    (insert (org-bookmarks-extractor--to-html-wrapper
             (org-bookmarks-extractor--walk
              (org-bookmarks-extractor-parse org-file))))
    (write-file html-file)))

(defun org-bookmarks-extractor-extract ()
  "Extract bookmarks."
  (interactive)
  (let* ((cur-file (buffer-file-name))
         (html-file
          (or org-bookmarks-extractor-html-file
              (file-name-with-extension
               (concat (file-name-as-directory (file-name-directory cur-file))
                       (file-name-base cur-file))
               "html"))))
    (unless (eq major-mode 'org-mode)
      (user-error "Not a org-mode file"))
    (org-bookmarks-extractor--extract cur-file html-file)))

;;;; Footer

(provide 'org-bookmarks-extractor)

;;; org-bookmarks-extractor.el ends here
