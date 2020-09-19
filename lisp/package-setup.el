;;; Settings for package.el pkg manager

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(unless package--initialized (package-initialize))

;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; (setq use-package-always-ensure t)
(setq package-check-signature nil)

(provide 'package-setup)
