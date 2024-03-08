;;; Setup for projectile mode.

(use-package projectile
  :disabled t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (cond
   ((eq pm-completion-system 'ido)
    (setq projectile-completion-system 'ido))
   ((eq pm-completion-system 'helm)
    (use-package helm-projectile
      :config
      ;; Bug opens files for current project
      ;; instead of new project when switching
      ;; (helm-projectile-on)
      )
    (setq projectile-completion-system 'helm))
   ((eq pm-completion-system 'ivy)
    (use-package counsel-projectile
      :config
      (counsel-projectile-mode 1))
    (setq projectile-completion-system 'ivy)))
  (setq projectile-enable-caching t)
  :delight '(:eval (concat " " (projectile-project-name))))

(use-package persp-projectile
  :bind (:map projectile-command-map
              ("." . projectile-persp-switch-project)))

(provide 'projectile-setup)
