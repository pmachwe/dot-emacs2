;;; Setup nano emacs

(use-package nano
  :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs")
  :init
  ;; font etc need to be set before loading
  (setq nano-font-size 11)
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  ;; nano override C-c r which I used for roam
  (global-unset-key (kbd "C-c r"))
  (global-set-key (kbd "C-c C-f") 'recentf-open-files)
  ;; nano overrides faces and bold and italics do not
  ;; comme out in org-mode
  (set-face-attribute 'italic nil
                      :slant 'oblique
                      :foreground nano-color-foreground
                      :weight 'normal
                      :underline nil)

  (set-face-attribute 'bold nil
                      :weight 'ultra-bold
                      :underline nil)
)

(provide 'nano-setup)
