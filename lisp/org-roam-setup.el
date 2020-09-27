(use-package org-roam
  :bind (("C-c r f" . org-roam-find-file)
         ("C-c r i" . org-roam-insert)
         ("C-c r t" . org-roam-dailies-today)
         ("C-c r y" . org-roam-dailies-yesterday)
         ("C-c r r" . org-roam))
  :config
  (setq org-roam-directory "~/org-roam")
  (add-to-list 'org-agenda-files "~/org-roam")
  (add-to-list 'org-agenda-files "~/org-roam/dailies")
  (setq org-roam-dailies-capture-templates '(("d" "daily" plain #'org-roam-capture--get-point
                                              "\n\n* Tasks\n\n* Discussions"
                                              :immediate-finish t
                                              :file-name "dailies/daily-%<%Y-%m-%d>"
                                              :head "#+title: daily-%<%Y-%m-%d>"))))


(use-package deft
  :disabled t
  :config
  (setq deft-directory "~/org-roam")
  (setq deft-extensions '("org"))
  (setq deft-default-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0)
  ;;key to launch deft
  (global-set-key (kbd "C-c d") 'deft))

(provide 'org-roam)
