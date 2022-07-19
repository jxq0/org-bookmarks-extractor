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
                            (org-element-interpret-data (org-element-contents data)))))
               (list (org-bookmarks-extractor-url-create :title title :url raw-link))))

      (_ (mapcan #'org-bookmarks-extractor--walk contents)))))

(defun org-bookmarks-extractor--to-html (data)
  "Convert DATA returned by `org-bookmarks-extractor--walk' into html."
  (let* ((title (format "<h3>%s</h3>" (car data)))
         (links-data (car (nth 1 data)))
         (links (if links-data
                    (mapconcat
                     (lambda (x)
                       (format "<dt><a href=\"%s\">%s</a></dt>"
                               (org-bookmarks-extractor-url-url x)
                               (org-bookmarks-extractor-url-title x)))
                     links-data "\n")
                  ""))
         (children-data (cdr (nth 1 data)))
         (children (mapconcat
                    (lambda (x)
                      (format "<dt>%s</dt>" (org-bookmarks-extractor--to-html x)))
                    children-data "\n"))
         (result (format "%s\n<dl>%s\n%s</dl>" title links children)))
    result))

(defun org-bookmarks-extractor--extract (org-file html-file)
  "Extract ORG-FILE into HTML-FILE."
  (with-temp-buffer
    (insert (org-bookmarks-extractor--to-html
             (org-bookmarks-extractor--walk (org-bookmarks-extractor-parse org-file))))
    (write-file html-file)))

(defun org-bookmarks-extractor-extract ()
  "Extract bookmarks."
  (interactive "P")
  )

;;;; Footer

(provide 'org-bookmarks-extractor)

;;; org-bookmarks-extractor.el ends here
