(defgroup pm-setup nil
  "Add personal customizations to this group.")

(defcustom pm-package-manager 'straight
  "The completion system to be used."
  :group 'pm-setup
  :type '(radio
          (const :tag "package" package)
          (const :tag "straight" straight)))

(defcustom pm-completion-system 'selectrum
  "The completion system to be used."
  :group 'pm-setup
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Selectrum" selectrum)
          (const :tag "Default" default)))

(defcustom pm-autocomplete-framework 'company
  "The autocomplete framework to use."
  :group 'pm-setup
  :type '(radio
          (const :tag "company" company)
          (const :tag "auto-complete" auto-complete)))

(defcustom pm-lsp-mode nil
  "LSP mode to use.")

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;; Load packages
(add-to-list 'load-path
	     (concat user-emacs-directory "/lisp"))

(cond
 ((eq pm-package-manager 'package)
  (load "package-setup"))
 ((eq pm-package-manager 'straight)
  (load "straight-setup")))

(load "themes-setup")
(load "basic-setup")

(cond
 ((eq pm-completion-system 'ido)
  (load "ido-setup"))
 ((eq pm-completion-system 'helm)
  (load "helm-setup"))
 ((eq pm-completion-system 'ivy)
  (load "ivy-setup"))
 ((eq pm-completion-system 'selectrum)
  (load "selectrum-setup")))

(cond
 ((eq pm-autocomplete-framework 'company)
  (load "company-setup"))
 ((eq pm-autocomplete-framework 'auto-complete)
  (load "auto-complete-setup")))

(load "projectile-setup")
(load "tools-setup")
(load "magit-setup")
(load "ediff-setup")
(load "p4-setup")
(load "prog-setup")
(if pm-lsp-mode
    (load "lsp-setup")
  (load "python-setup"))
(load "org-setup")
(load "org-roam-setup")
(load "personal-keybindings")
(load "functions")
;; May not exist
(load "local-setup" t)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my--file-name-handler-alist)))
