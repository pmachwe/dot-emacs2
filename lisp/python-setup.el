;;; Setup for python
;;; Mostly taken from: https://realpython.com/emacs-the-best-python-editor
;;; TODO: Move ensure-system-package under condtion of whether the
;;;       package is not already installed
;;; NOTE: elpy may pick a differnt version of python and some autocompletion
;;;       may not work so set elpy-rpc-python-command

(use-package use-package-ensure-system-package)

(use-package elpy
  ;; :ensure-system-package
  ;; ((elpy . "pip install elpy")
  ;;  (jedi . "pip install jedi")
  ;;  (rope . "pip install rope")
  ;;  (black . "pip install black")
  ;;  (rope . "pip install rope"))
  :config
  ;; Enable elpy
  (elpy-enable)

  ;; Use IPython for REPL
  (cond
   ((executable-find "jupyter")
    (setq python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt"
          python-shell-prompt-detect-failure-warning nil)
    (add-to-list 'python-shell-completion-native-disabled-interpreters
                 "jupyter"))
   ((executable-find "ipython3")
    (setq python-shell-interpreter "ipython3"))
   ((executable-find "ipython")
    (setq python-shell-interpreter "ipython"))))

(use-package blacken
  :hook (python-mode . blacken-mode))

;; Can update to flycheck from default flymake
(use-package flycheck
  :disabled
  :hook python-mode
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Jupyter notebook control within Emacs
;; TODO: Need to evaluate
(use-package ein
  :disabled
  :hook python-mode)

;;; NOTE: elpy-company-backend gets added so
;;;       this should not be required
;; (use-package company-jedi
;;   :disabled
;;   :config
;;   (defun my/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'my/python-mode-hook))

(provide 'python-setup)
