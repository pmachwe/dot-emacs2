;;; Setup for straight.el package manager

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-check-for-modifications '(check-on-save find-when-checking))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(setq use-package-expand-minimally t)

(add-to-list 'straight-built-in-pseudo-packages 'dired)
(add-to-list 'straight-built-in-pseudo-packages 'dired-async)
(add-to-list 'straight-built-in-pseudo-packages 'replace)
(add-to-list 'straight-built-in-pseudo-packages 'isearch)
(add-to-list 'straight-built-in-pseudo-packages 'abbrev)
(add-to-list 'straight-built-in-pseudo-packages 'ediff)

(provide 'straight-setup)
