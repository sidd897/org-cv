;;; ox-moderncv.el --- LaTeX moderncv Back-End for Org Export Engine -*- lexical-binding: t; -*-

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
;; This library implements a LaTeX moderncv back-end, derived from the
;; LaTeX one.

;;; Code:
(require 'cl-lib)
(require 'ox-latex)
(require 'org-cv-utils)

;; Install a default set-up for moderncv export.
(unless (assoc "moderncv" org-latex-classes)
  (add-to-list 'org-latex-classes
               '("moderncv"
                 "\\documentclass{moderncv}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


;;; User-Configurable Variables

(defgroup org-export-cv nil
  "Options specific for using the moderncv class in LaTeX export."
  :tag "Org moderncv"
  :group 'org-export
  :version "25.3")

;;; Define Back-End
(org-export-define-derived-backend 'moderncv 'latex
  :menu-entry
  '(?l 1
       ((?C "As LaTeX buffer (modernCV)" org-moderncv-export-as-latex)
        (?c "As LaTex file (modernCV)" org-moderncv-export-to-latex)
        (?M "As PDF file (modernCV)" org-moderncv-export-to-pdf)
        (?m "As PDF file (modernCV)" org-moderncv-export-to-pdf-and-open)))
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "moderncv" t)
    (:cvstyle "CVSTYLE" nil "classic" t)
    (:cvstyle-options "CVSTYLE_OPTIONS" nil nil t)
    (:cvcolor "CVCOLOR" nil "blue" t)
    (:hints-column-width "HINTS_COLUMN_WIDTH" nil nil t)
    (:mobile "MOBILE" nil nil parse)
    (:homepage "HOMEPAGE" nil nil parse)
    (:address "ADDRESS" nil nil newline)
    (:photo "PHOTO" nil nil parse)
    (:photo-height "PHOTO_HEIGHT" nil nil t)
    (:photo-frame-width "PHOTO_FRAME_WIDTH" nil nil t)
    (:gitlab "GITLAB" nil nil parse)
    (:github "GITHUB" nil nil parse)
    (:linkedin "LINKEDIN" nil nil parse)
    (:with-email nil "email" t t)
    (:firstname "FIRSTNAME" nil nil t)
    (:lastname "LASTNAME" nil nil t))
  :translate-alist '((template . org-moderncv-template)
                     (headline . org-moderncv-headline)
                     (item . org-moderncv-item)
                     (plain-list . org-moderncv-plain-list)))


;;;; Template
;;
;; Template used is similar to the one used in `latex' back-end,
;; excepted for the table of contents and moderncv themes.

(defun org-moderncv-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
        (spec (org-latex--format-spec info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
          (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (org-latex-make-preamble info)
     ;; cvcolor
     (let ((cvcolor (org-export-data (plist-get info :cvcolor) info)))
       (when (not (string-empty-p cvcolor)) (format "\\moderncvcolor{%s}\n" cvcolor)))
     ;; cvstyle
     (let ((cvstyle (org-export-data (plist-get info :cvstyle) info))
           (options (org-export-data (plist-get info :cvstyle-options) info)))
       (when cvstyle (if (not (string= "" options))
                         (format "\\moderncvstyle[%s]{%s}\n" options cvstyle)
                       (format "\\moderncvstyle{%s}\n" cvstyle))))
     ;; hintscolumnwidth
     (when-let* ((hint-col-width (plist-get info :hints-column-width)))
       (format "\\setlength{\\hintscolumnwidth}{%s}" hint-col-width))
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (when (integerp sec-num)
         (format "\\setcounter{secnumdepth}{%d}\n" sec-num)))
     ;; Author. If FIRSTNAME or LASTNAME are not given, try to deduct
     ;; their values by splitting AUTHOR on white space.
     (let* ((author (split-string (org-export-data (plist-get info :author) info)))
            (first-name-prop (org-export-data (plist-get info :firstname) info))
            (last-name-prop (org-export-data (plist-get info :lastname) info))
            (first-name (or (org-string-nw-p first-name-prop) (car author)))
            (last-name (or (org-string-nw-p last-name-prop) (cadr author))))
       (format "\\name{%s}{%s}\n" first-name last-name))
     ;; photo
     (let ((photo (org-export-data (plist-get info :photo) info))
           (height (plist-get info :photo-height))
           (frame-width (plist-get info :photo-frame-width)))
       (when (org-string-nw-p photo)
         (cond ((and height frame-width) (format "\\photo[%s][%s]{%s}\n" height frame-width photo))
               (height (format "\\photo[%s]{%s}\n" height photo))
               (frame-width (format "\\photo[64pt][%s]{%s}\n" frame-width photo))
               (t (format "\\photo{%s}\n" photo)))))
     ;; email
     (let ((email (and (plist-get info :with-email)
                       (org-export-data (plist-get info :email) info))))
       (when (org-string-nw-p email)
         (format "\\email{%s}\n" email)))
     ;; phone
     (let ((mobile (org-export-data (plist-get info :mobile) info)))
       (when (org-string-nw-p mobile)
         (format "\\phone[mobile]{%s}\n" mobile)))
     ;; homepage
     (let ((homepage (org-export-data (plist-get info :homepage) info)))
       (when (org-string-nw-p homepage)
         (format "\\homepage{%s}\n" homepage)))
     ;; address
     (let ((address (org-export-data (plist-get info :address) info)))
       (when (org-string-nw-p address)
         (format "\\address%s\n" (mapconcat (lambda (line)
                                              (format "{%s}" line))
                                            (split-string address "\n") ""))))
     (mapconcat (lambda (social-network)
                  (let ((network (org-export-data
                                  (plist-get info (car social-network)) info)))
                    (when (org-string-nw-p network)
                      (format "\\social[%s]{%s}\n"
                              (nth 1 social-network) network))))
                '((:github "github")
                  (:gitlab "gitlab")
                  (:linkedin "linkedin"))
                "")

     ;; Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))

     ;; Title and subtitle.
     (let* ((subtitle (plist-get info :subtitle))
            (formatted-subtitle
             (when subtitle
               (format (plist-get info :latex-subtitle-format)
                       (org-export-data subtitle info))))
            (separate (plist-get info :latex-subtitle-separate)))
       (concat
        (format "\\title{%s%s}\n" title
                (if separate "" (or formatted-subtitle "")))
        (when (and separate subtitle)
          (concat formatted-subtitle "\n"))))
     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
            (format-spec template spec)))
     ;; Document start.
     "\\begin{document}\n\n"
     ;; Title command.
     (let* ((title-command (plist-get info :latex-title-command))
            (command (and (stringp title-command)
                          (format-spec title-command spec))))
       (org-element-normalize-string
        (cond ((not (plist-get info :with-title)) nil)
              ((string= "" title) nil)
              ((not (stringp command)) nil)
              ((string-match "\\(?:[^%]\\|^\\)%s" command)
               (format command title))
              (t command))))
     ;; Document's body.
     contents
     ;; Creator.
     (and (plist-get info :with-creator)
          (concat (plist-get info :creator) "\n"))
     ;; Document end.
     "\\end{document}")))


(defun org-moderncv--format-cventry (headline contents info)
  "Format HEADLINE as as cventry.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (let* ((entry (org-cv-utils--parse-cventry headline info))
         (note (or (org-element-property :NOTE headline) "")))
    (format "\\cventry{%s}{%s}{%s}{%s}{%s}{%s}\n"
            (alist-get 'date entry)
            (alist-get 'title entry)
            (alist-get 'host entry)
            (alist-get 'location entry)
            note contents)))

(defconst org-moderncv-cvcolumns-alist nil
  "Internal variable for keeping count of number of `cvcolumn' type
headlines under a headline. It stores an alist of format

'((PARENT-1 . NUMBER-OF-CVCOLUMN-1)
  (PARENT-2 . NUMBER-OF-CVCOLUMN-2) ...),

where PARENT-1, PARENT-2, ... are a unique reference generated by
`org-export-get-reference',")

(defun org-moderncv--format-cvitem (headline contents info)
  "Format HEADLINE as cvitem.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."  
  (let* ((entry (org-cv-utils--parse-cvitem headline info)))
      (format "\\cvitem{%s}{%s}\n%s"
              (alist-get 'description entry)
              (alist-get 'items entry)
              contents)))

(defun org-moderncv--format-cvcolumn (headline contents info)
  "Format HEADLINE as as cvcolumn.
CONTENTS holds the contents of the headline.  INFO is a plist used
as a communication channel."
  (if-let* ((parent (org-element-parent-element headline))
            (title (org-export-get-reference parent info))
            (entry (org-cv-utils--parse-cvcolumn headline info))
            (cvcolumn (if (alist-get 'width entry)
                          (format " \\cvcolumn[%s]{%s}{%s}\n"
                              (alist-get 'width entry)
                              (alist-get 'category entry)
                              (alist-get 'content entry))
                        (format " \\cvcolumn{%s}{%s}\n"
                                (alist-get 'category entry)
                                (alist-get 'content entry))))
            (value? (alist-get title org-moderncv-cvcolumns-alist)))
      (progn
        (setf (alist-get title org-moderncv-cvcolumns-alist) (- value? 1))
        (concat cvcolumn
                (and (eq (alist-get title org-moderncv-cvcolumns-alist) 1)
                     "\\end{cvcolumns}\n")))
    (let ((cols (org-cv-utils--cvcolumns-under-node parent info)))
      (push `(,title . ,cols) org-moderncv-cvcolumns-alist)
      (concat "\\begin{cvcolumns}\n"
              cvcolumn
              (and (eq cols 1) "\\end{cvcolumns}\n")))))

;;;; Plain-list
(defun org-moderncv-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element into moderncv code. CONTENTS is the
contents of the item. INFO is a plist used as a communication channel."
  (if-let* ((parent (org-element-lineage plain-list 'headline))
            (cvlistitem-p (member
                           "cvlistitem"
                           (org-export-get-tags parent info))))
      (format "%s" contents)
    (org-export-with-backend 'latex plain-list contents info)))

;;;; Item
(defun org-moderncv-item (item contents info)
  "Transcode ITEM element into moderncv code. CONTENTS is the contents of
the item. INFO is a plist used as a communication channel"
  (if-let* ((parent (org-element-lineage item 'headline))
            (cvlistitem-p (member
                           "cvlistitem"
                           (org-export-get-tags parent info))))
      (format "\\cvlistitem{%s}\n" contents)
    (org-export-with-backend 'latex item contents info)))

;;;; Headline
(defun org-moderncv-headline (headline contents info)
  "Transcode HEADLINE element into moderncv code.
CONTENTS is the contents of the headline.  INFO is a plist used
as a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let ((environment (cons (org-element-property :CV_ENV headline)
                             (org-export-get-tags headline info))))
      (cond
       ((member "cventry" environment)  ; is a cventry
        (org-moderncv--format-cventry headline contents info))
       ((member "cvitem" environment)   ; is a cvitem
        (org-moderncv--format-cvitem headline contents info))
       ((member "cvcolumn" environment) ; is a cvcolumn
        (org-moderncv--format-cvcolumn headline contents info))
       ;; everything else
       ((org-export-with-backend 'latex headline contents info))))))


;;;###autoload
(defun org-moderncv-export-as-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a ModernCV buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org MODERNCV Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (setq org-moderncv-cvcolumns-alist nil) ; reset variable
  (org-export-to-buffer 'moderncv "*Org MODERNCV Export*"
    async subtreep visible-only body-only ext-plist
    (if (fboundp 'major-mode-remap)
        (major-mode-remap 'latex-mode)
      #'LaTeX-mode)))

;;;###autoload
(defun org-moderncv-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a ModernCV file (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (setq org-moderncv-cvcolumns-alist nil) ; reset variable
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'moderncv file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-moderncv-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a ModernCV file (PDF).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (setq org-moderncv-cvcolumns-alist nil) ; reset variable
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'moderncv file
      async subtreep visible-only body-only ext-plist
      #'org-latex-compile)))

(defun org-moderncv-export-to-pdf-and-open
    (async subtree visible-only body-only)
  "Export current buffer as ModernCV file (PDF), and open it in a different
window."
  (if async (org-moderncv-export-to-pdf t subtree visible-only body-only)
	(org-open-file
     (org-moderncv-export-to-pdf nil subtree visible-only body-only))))

(provide 'ox-moderncv)
;;; ox-moderncv ends here
