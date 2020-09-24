;;; Setup themes etc

(use-package doom-themes
  :disabled
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-operandi-theme)

(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package telephone-line
  :if window-system
  :init
  (telephone-line-mode t))

;; Disabling for now as debugging needed
;; Not changing the theme cleanly
(use-package theme-changer
  :disabled
  :config
  (setq calendar-location-name "Bangalore, IND") 
  (setq calendar-latitude 12.97)
  (setq calendar-longitude 77.59)
  (change-theme 'modus-operandi 'nord))

(provide 'themes-setup)
