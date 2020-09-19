;;; Setup themes etc

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-operandi-theme
  :config
  (load-theme 'modus-operandi t))

(use-package telephone-line
  :if window-system
  :requires modus-operandi-theme
  :init
  (telephone-line-mode t))

(provide 'themes-setup)
