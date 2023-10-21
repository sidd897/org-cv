(defvar cv-cwd default-directory
  "remember the current directory, find-file changes it")
(defvar cv-workdir "/tmp/org-cv-exports/")

(with-current-buffer (find-file-noselect "/tmp/install-org.el")
  (eval-buffer))

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)

(use-package ox-moderncv
  :load-path cv-cwd
  :init
  (require 'ox-moderncv)
  (require 'ox-altacv)
  (require 'ox-hugocv)
  (require 'ox-awesomecv))

(defun export-latex (backend file)
  (let ((workfile (concat cv-workdir file))
        (outfile (concat cv-workdir file ".tex"))
        (pdffile (concat cv-workdir file ".pdf")))
    (message (format "%s exists: %s" workfile (file-exists-p workfile)))
    (with-current-buffer
        (find-file-noselect workfile)
      (org-mode)
      (org-export-to-file backend outfile)
      (shell-command (format "lualatex %s" outfile) "*Messages*" "*Messages*")
      (copy-file pdffile (concat cv-cwd "/doc/static/" (concat file ".pdf")) t))))

(let ((readme (concat cv-cwd "readme.org")))
  (make-directory cv-workdir t)
  (with-current-buffer
      (find-file-noselect readme)
    (cd cv-workdir)
    (org-babel-tangle)))

(copy-file (concat cv-cwd "doc/smile.png") cv-workdir t)
(make-directory (concat cv-cwd "/doc/static/") t)
(export-latex 'altacv "altacv.org")
(export-latex 'moderncv "moderncv.org")
(export-latex 'awesomecv "awesomecv.org")
(export-latex 'awesomecv "awesome-letter.org")
