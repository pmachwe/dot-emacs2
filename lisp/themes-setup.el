;;; Setup themes etc

(use-package doom-themes
  :disabled
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-operandi-theme
  :disabled t
  :defer 5)

(use-package nord-theme
  :disabled t
  :if window-system
  :config
  (load-theme 'nord t))

(use-package telephone-line
  :disabled t
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

(use-package bespoke-themes
  :disabled t
  :straight (:host github :repo "mclear-tools/bespoke-themes" :branch "main")
  :config
  ;; Set header line
  (setq bespoke-set-mode-line 'header)
  ;; Set mode line height
  (setq bespoke-set-mode-line-size 3)
  ;; Show diff lines in modeline
  (setq bespoke-set-git-diff-mode-line t)
  ;; Set mode-line cleaner
  (setq bespoke-set-mode-line-cleaner t)
  ;; Set evil cursor colors
  (setq bespoke-set-evil-cursors t)
  ;; Use mode line visual bell
  (setq bespoke-set-visual-bell t)
  ;; Set use of italics
  (setq bespoke-set-italic-comments t
        bespoke-set-italic-keywords t)
  ;; Set variable pitch
  (setq bespoke-set-variable-pitch t)
  ;; Set initial theme variant
  (setq bespoke-set-theme 'dark)
  ;; Load theme
  (load-theme 'bespoke t)

  ;; Vertical window divider
  (use-package frame
    :straight (:type built-in)
    :custom
    (window-divider-default-right-width 12)
    (window-divider-default-bottom-width 1)
    (window-divider-default-places 'right-only)
    (window-divider-mode t))

  ;; Make sure new frames use window-divider
  (add-hook 'before-make-frame-hook 'window-divider-mode)

  ;; Make a clean & minimalist frame
  (use-package frame
    :straight (:type built-in)
    :config
    (setq-default default-frame-alist
                  (append (list
                           '(font . "SF Mono:style=medium:size=15") ;; NOTE: substitute whatever font you prefer here
                           '(internal-border-width . 20)
                           '(left-fringe    . 0)
                           '(right-fringe   . 0)
                           '(tool-bar-lines . 0)
                           '(menu-bar-lines . 0)
                           '(vertical-scroll-bars . nil))))
    (setq-default window-resize-pixelwise t)
    (setq-default frame-resize-pixelwise t))

  ;; Dim inactive windows
  (use-package dimmer
    :straight (:host github :repo "gonewest818/dimmer.el")
    :hook (after-init . dimmer-mode)
    :config
    (setq dimmer-fraction 0.5)
    (setq dimmer-adjustment-mode :foreground)
    (setq dimmer-use-colorspace :rgb)
    (setq dimmer-watch-frame-focus-events nil)
    (dimmer-configure-which-key)
    (dimmer-configure-magit)
    (dimmer-configure-posframe)))


(use-package splash-screen
  :straight (emacs-splash :host github :repo "rougier/emacs-splash"))

(provide 'themes-setup)
