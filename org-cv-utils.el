;;; org-cv-utils.el --- Common utility functions for CV exporters -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Oscar Najera <hi AT oscarnajera.com DOT com>
;; Keywords: org, wp, tex

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library implements some utility functions

;;; Code:
(require 'org)
(require 'ox)
(require 'org-element)

(defun org-cv-utils-org-timestamp-to-shortdate (date_str)
  "Format orgmode timestamp DATE_STR  into a short form date.
Other strings are just returned unmodified

e.g. <2012-08-12 Mon> => Aug 2012
today => today"
  (if (string-match (org-re-timestamp 'all) date_str)
      (let* ((dte (org-parse-time-string date_str))
             (month (nth 4 dte))
             (year (nth 5 dte))) ;;'(02 07 2015)))
        (concat
         (calendar-month-name month 'abbreviate) " " (number-to-string year)))
    date_str))

(defun org-cv-utils--format-time-window (from-date to-date)
  "Join date strings in a time window.
FROM-DATE -- TO-DATE
in case TO-DATE is nil return Present.
If both dates are the same, return just FROM-DATE"
  (let ((from (org-cv-utils-org-timestamp-to-shortdate from-date))
        (to (if (not to-date) "Present"
              (org-cv-utils-org-timestamp-to-shortdate to-date))))
    (if (or (string= from to))
        from
      (concat from " -- " to))))

(defun org-cv-utils--parse-cventry (headline info)
  "Return alist describing the entry in HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((title (org-export-data (org-element-property :title headline) info))
         (date (org-element-property :DATE headline))
         (from-date (or (org-element-property :FROM headline)
                        date
                        (user-error "No FROM property provided for cventry %s" title)))
         (to-date (or (org-element-property :TO headline) date))
         (host (or (org-element-property :HOST headline)
                   (org-element-property :ORGANIZATION headline)
                   (org-element-property :INSTITUTION headline)
                   (org-element-property :SCHOOL headline)
                   (org-element-property :EMPLOYER headline)
                   (org-element-property :EVENT headline) "")))
    `((title . ,title)
      (date . , (org-cv-utils--format-time-window from-date to-date))
      (host . ,host)
      (location . ,(or (org-element-property :LOCATION headline) ""))
      (image . ,(org-element-property :IMAGE headline)))))

(defun org-cv-utils--cvcolumns-under-node (node info)
  "Count the number of headlines having tag `cvcolumn' under NODE. INFO is
a plist used as a communication channel"
  (length
   (let ((contents (org-element-contents node)))
     (seq-filter
      (lambda (content)
        (and (eq (org-element-type content) 'headline)
             (member "cvcolumn" (org-export-get-tags content info))))
      contents))))

(defun org-cv-utils--parse-cvitem (headline info)
  "Return alist describing the entry in `cvitem' HEADLINE. INFO is a plist
used as a communication channel"
  (let* ((descr (org-export-data (org-element-property :title headline) info))
         (items (or (org-element-property :ITEMS headline)
                    (user-error "No ITEMS provided for cvitem %s" title))))
    `((description . ,descr)
      (items . ,items))))

(defun org-cv-utils--parse-cvcolumn (headline info)
  "Return alist describing the entry in `cvcolumn' HEADLINE. INFO is a plist
used as a communication channel"
  (let* ((category (org-export-data (org-element-property :title headline) info))
         (width (org-element-property :WIDTH headline))
         (job (org-element-property :JOB headline))
         (dept (org-export-data (org-element-property :DEPARTMENT headline) info))
         (school (org-export-data (org-element-property :SCHOOL headline) info))
         (phone (org-export-data (org-element-property :PHONE headline) info))
         (email (org-export-data (org-element-property :EMAIL headline) info))
         (content (or (replace-regexp-in-string "\\(\\\\\\\\\\)+" "\\\\\\\\"
                       (string-trim
                        (string-join
                         `(,(and job (format "\\textit{%s}" job))
                           ,dept
                           ,school
                           ,phone
                           ,email
                           ,(org-export-data (org-element-property :CONTENT headline) info)) "\\\\")
                        "\\(\\\\\\\\\\)+"
                        "\\(\\\\\\\\\\)+"))
                      (user-error "No CONTENT provided for cvcolumn %s" title))))
    `((category . ,category)
      (content . ,content)
      (width . ,width))))

(provide 'org-cv-utils)
;;; org-cv-utils.el ends here
