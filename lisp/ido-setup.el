;;; Setup for ido-mode and other related packages.

(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-show-dot-for-dired t)       ; pwd as first candidate
  (setq ido-enable-dot-prefix t)        ; show hidden files when . entered
  (setq ido-auto-merge-work-directories-length -1) ; disable searching in recent dirs

  (defun pm/ido-kill-ring()
    "Show the kill-ring through ido interface."
    (interactive)
    (let (orig-sep ido-separator)
      (setq ido-separator "\n....\n")
      (insert
       (completing-read "Yank: " kill-ring nil t))
      (setq ido-separator orig-sep)))

  (defun pm/recentf()
    "Show the recentf files through ido interface."
    (interactive)
    (let ((orig-sep ido-separator)
          (files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file
       (completing-read "Open recent file: " files nil t))))

  :bind (("C-x C-f" . ido-find-file)
         ("M-j"     . ido-find-file)
         ("C-x b"   . ido-switch-buffer)
         ("M-o"     . ido-switch-buffer)
         ("M-y"     . pm/ido-kill-ring)
         ("C-x C-r" . pm/recentf)))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-show-count t)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

;; Note - have flx and flx-ido after ido-vertical-mode
;; otherwise it does not get activated
(use-package flx)

(use-package flx-ido
  :requires (flx ido)
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t))

(use-package smex
  :bind(("M-x" . smex)
        ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package idomenu
  :requires ido
  :bind ("C-c /" . idomenu))

;; Fallback
(require 'icomplete)
(icomplete-mode 1)

(provide 'ido-setup)
