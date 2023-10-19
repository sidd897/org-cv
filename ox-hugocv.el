;;; ox-hugocv.el --- LaTeX hugocv Back-End for Org Export Engine -*- lexical-binding: t; -*-

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
;; This library implements a LaTeX hugocv back-end, derived from the
;; LaTeX one.

;;; Code:
(require 'ox-hugo)
(require 'dash)
(require 'org-cv-utils)

;;; User-Configurable Variables

(defgroup org-export-hugocv nil
  "Options for exporting Org mode files to Hugo-compatible Markdown"
  :tag "Org Export Hugo CV"
  :group 'org-export
  :version "25.3")

;;; Define Back-End
(org-export-define-derived-backend 'hugocv 'hugo
  :options-alist
  '(
    (:mobile "MOBILE" nil nil parse)
    (:homepage "HOMEPAGE" nil nil parse)
    (:address "ADDRESS" nil nil newline)
    (:photo "PHOTO" nil nil parse)
    (:gitlab "GITLAB" nil nil parse)
    (:github "GITHUB" nil nil parse)
    (:linkedin "LINKEDIN" nil nil parse)
    (:with-email nil "email" t t)
    )
  :translate-alist '((headline . org-hugocv-headline)))

(defun org-hugocv--entry-with-icon (field entry)
  "HTML entry for given FIELD when it is specified in ENTRY."
  (cl-ecase field
    (employer
     (-some->> (alist-get 'employer entry)
       (format "<i class=\"fa fa-building\"></i>%s<br/>")))
    (date
     (-some->>
         (org-cv-utils--format-time-window (alist-get 'from-date entry) (alist-get 'to-date entry))
       (format "<i class=\"fa fa-calendar\"></i>%s")))
    (location
     (-some->> (alist-get 'location entry)
       (org-string-nw-p)
       (format "<i class=\"fa fa-map-marker\"></i>%s")))))

(defun org-hugocv--format-cventry (headline contents info)
  "Format HEADLINE as as cventry.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (let* ((entry (org-cv-utils--parse-cventry headline info))
         (loffset (string-to-number (plist-get info :hugo-level-offset))) ;"" -> 0, "0" -> 0, "1" -> 1, ..
         (level (org-export-get-relative-level headline info))
         (title (concat (make-string (+ loffset level) ?#) " " (alist-get 'title entry))))
    (format "<div class=\"cv-entry\">
\n%s

%s

%s

</div>" title
(mapconcat (lambda (field) (org-hugocv--entry-with-icon field entry))
           '(employer date location)
           "\n")
contents)))



;;;; Headline
(defun org-hugocv-headline (headline contents info)
  "Transcode HEADLINE element into hugocv code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((environment (let ((env (org-element-property :CV_ENV headline)))
                         (or (org-string-nw-p env) "block"))))
      (cond
       ;; is a cv entry
       ((equal environment "cventry")
        (org-hugocv--format-cventry headline contents info))
       ((org-export-with-backend 'hugo headline contents info))))))

(provide 'ox-hugocv)
;;; ox-hugocv ends here
