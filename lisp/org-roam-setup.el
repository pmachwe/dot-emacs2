;;; Settings for org-roam and other note taking packages

(use-package org-roam
  :bind (("C-c r f" . org-roam-find-file)
         ("C-c r i" . org-roam-insert)
         ("C-c r t" . org-roam-dailies-find-today)
         ("C-c r y" . org-roam-dailies-find-yesterday)
         ("C-c r T" . org-roam-dailies-capture-today)
         ("C-c r r" . org-roam))
  :config
  (setq org-roam-directory "~/org-roam")
  (setq org-return-follows-link t)
  (setq org-roam-agenda-dirs (list org-roam-directory (f-join org-roam-directory "dailies")))
  (if (boundp 'org-agenda-files)
      (setq org-agenda-files (append org-agenda-files org-roam-agenda-dirs))
    (setq org-agenda-files org-roam-agenda-dirs))
  (setq org-roam-dailies-capture-templates '(("d" "daily" plain #'org-roam-capture--get-point
                                              "%?"
                                              :immediate-finish t
                                              :file-name "dailies/daily-%<%B-%m-%Y>"
                                              :head "#+title: %<%B %m, %Y>\n\n* Tasks\n\n* Discussions"))))


(use-package deft
  :after org-roam
  :config
  (setq deft-directory "~/org-roam")
  (setq deft-extensions '("org"))
  (setq deft-default-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0)
  (setq deft-recursive t)
  ;;key to launch deft
  (global-set-key (kbd "C-c r d") 'deft))

(provide 'org-roam)
