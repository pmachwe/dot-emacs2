;;; Setup nano emacs

(use-package nano
  :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs")
  :init
  (setq nano-font-size 12)
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  ;; nano override C-c r which I used for roam
  (global-set-key (kbd "C-c C-f") 'recentf-open-files))

(provide 'nano-setup)
