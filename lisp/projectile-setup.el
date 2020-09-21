;;; Setup for projectile mode.

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (cond
   ((eq pm-completion-system 'ido)
    (setq projectile-completion-system 'ido))
   ((eq pm-completion-system 'helm)
    (setq projectile-completion-system 'helm))
   ((eq pm-completion-system 'ivy)
    (use-package counsel-projectile
      :config
      (counsel-projectile-mode 1))
    (setq projectile-completion-system 'ivy)))
  (setq projectile-enable-caching t)
  :delight '(:eval (concat " " (projectile-project-name))))

(use-package persp-projectile
  :bind (:map projectile-mode-map
              ("C-c p ." . projectile-persp-switch-project)))

(provide 'projectile-setup)
