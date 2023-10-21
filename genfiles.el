(defvar cv-cwd default-directory
  "remember the current directory, find-file changes it")
(defvar cv-workdir "/tmp/org-cv-exports/")

(with-current-buffer (find-file-noselect "/tmp/install-org.el")
  (emacs-lisp-byte-compile-and-load))

(add-to-list 'load-path cv-cwd)

(require 'ox-moderncv)
(require 'ox-altacv)
(require 'ox-awesomecv)

(let ((readme (concat cv-cwd "readme.org")))
  (make-directory cv-workdir t)
  (cd cv-workdir)
  (with-current-buffer
      (find-file-noselect readme)
    (org-babel-tangle)))

(copy-file (concat cv-cwd "doc/smile.png") cv-workdir t)

(defun export-latex (backend file)
  (let ((workfile (concat cv-workdir file))
        (outfile (concat cv-workdir file ".tex")))
    (message (format "%s exists: %s" workfile (file-exists-p workfile)))
    (find-file workfile)
    (org-mode)
    (org-export-to-file backend outfile)
    (shell-command (format "lualatex %s" outfile) "*Messages*" "*Messages*")
    (copy-file (concat file ".pdf") (concat cv-cwd "/doc/static/" (concat file ".pdf")) t)))

(make-directory (concat cwd "/doc/static/") t)
(export-latex 'altacv "altacv.org")
(export-latex 'moderncv "moderncv.org")
(export-latex 'awesomecv "awesomecv.org")
(export-latex 'awesomecv "awesome-letter.org")
