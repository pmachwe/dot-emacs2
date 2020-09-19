(defgroup pm-setup nil
  "Add personal customizations to this group.")

(defcustom pm-package-manager 'straight
  "The completion system to be used."
  :group 'pm-setup
  :type '(radio
          (const :tag "package" package)
          (const :tag "straight" straight)))

(defcustom pm-completion-system 'ido
  "The completion system to be used."
  :group 'pm-setup
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)))

(defcustom pm-autocomplete-framework 'company
  "The autocomplete framework to use."
  :group 'pm-setup
  :type '(radio
          (const :tag "company" company)
          (const :tag "ac" ac)))

;;;; Load packages
(add-to-list 'load-path
	     (concat user-emacs-directory "/lisp"))

(cond
 ((eq pm-package-manager 'package)
  (load "package-setup"))
 ((eq pm-package-manager 'straight)
  (load "straight-setup")))

(load "basic-setup")

(cond
 ((eq pm-completion-system 'ido)
  (load "ido-setup"))
 (eq pm-completion-system 'helm)
 (load "helm-setup")
 (eq pm-completion-system 'ivy)
  (load "ivy-setup"))

(cond
 ((eq pm-autocomplete-framework 'company)
  (load "company-setup"))
 ((eq pm-autocomplete-framework 'ac)
  (load "ac-setup")))
