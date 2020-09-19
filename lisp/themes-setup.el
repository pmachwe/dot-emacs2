;;; Setup themes etc

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-operandi-theme)

(use-package telephone-line
  :if window-system
  :init
  (telephone-line-mode t))

(provide 'themes-setup)
