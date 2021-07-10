;;; Settings for org-roam and other note taking packages

(use-package org-roam
  :config
  (setq org-roam-directory "~/org-roam")
  (setq org-return-follows-link t)
  (setq org-roam-agenda-dirs (list org-roam-directory (f-join org-roam-directory "dailies")))
  (setq org-startup-with-inline-images t)
  (if (boundp 'org-agenda-files)
      (setq org-agenda-files (append org-agenda-files org-roam-agenda-dirs))
    (setq org-agenda-files org-roam-agenda-dirs))
  (setq org-roam-dailies-capture-templates '(("d" "daily" plain #'org-roam-capture--get-point
                                              "%?"
                                              :immediate-finish t
                                              :file-name "dailies/daily-%<%B-%m-%Y>"
                                              :head "#+title: %<%B %m, %Y>\n\n* Tasks\n\n* Discussions"
                                              :unnarrowed t
                                              :jump-to-captured t)))
  
  (setq org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point
                                      :immediate-finish t
                                      :file-name "%<%Y%m%d%H%M%S>-${title}"
                                      :head "#+title: ${title}\n#+created: %u\n#+roam_tags:\n%?"
                                      :unnarrowed t
                                      :jump-to-captured t)
                                     ("t" "temp" plain #'org-roam-capture--get-point
                                      :immediate-finish t
                                      :file-name "temp/%<%Y%m%d%H%M%S>-${title}"
                                      :head "#+title: ${title}\n#+created: %u\n#+roam_tags: temp\n%?"
                                      :unnarrowed t
                                      :jump-to-captured t)))
  
  ;; Some functions to work on Windows
  (defcustom device-screenshot-dir nil
    "Directory where screenshots are to be found.")

  (defun is-image-file (file)
    (or (equal (f-ext file) "png")
        (equal (f-ext file) "jpg")))

  (defun latest-file (path)
    "Get latest file in PATH."
    (let (files)
      ;; (setq files (directory-files path 'full pat t))
      (setq files (f-files path 'is-image-file))
      (car (sort files #'file-newer-than-file-p))))

  (defun org-roam-insert-latest-screenshot()
    (interactive)
    (let* ((orig-file (latest-file device-screenshot-dir))
           (file-name (f-filename orig-file))
           (new-file-name (f-join org-roam-directory "images" file-name)))
      (f-copy orig-file new-file-name)
      (insert (concat "[[./" (f-relative new-file-name) "]]"))
      (org-display-inline-images)))

  :bind (("C-c r f" . org-roam-find-file)
         ("C-c r i" . org-roam-insert)
         ("C-c r t" . org-roam-dailies-find-today)
         ("C-c r y" . org-roam-dailies-find-yesterday)
         ("C-c r T" . org-roam-dailies-capture-today)
         ("C-c r r" . org-roam)
         ("C-c r y" . org-roam-insert-latest-screenshot)))

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

;; Disabling as does not seem to work on Win10
;; Evaluate and enable later.
(use-package org-download
  :disabled
  :config
  (setq-default org-download-method 'directory)
  (setq-default org-download-image-dir "~/org-roam/images")
  ;; (setq-default org-download-screenshot-method "i_view64 /capture=4 /convert=\"%s\"")
  (add-hook 'org-mode-hook 'org-download-enable)
  (add-hook 'dired-mode-hook 'org-download-enable))

(provide 'org-roam-setup)
