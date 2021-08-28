;;; Setup personal keybindings
;;; TODO: Setup a minor mode

(use-package emacs
  :bind (("M-h" . backward-kill-word)
         ("S-<f5>" . eshell)
         ("C-c M-!" . eshell-command)
         ("M-k" . kill-buffer-and-window)
         ("M-K" . kill-buffer)
         ("C-c ," . highlight-symbol-at-point)
         ("C-c C-," . unhighlight-regexp)
         ("C-M-o" . mode-line-other-window)
         ("C-c C-r" . revert-buffer)
         ("C-c |" . eval-region)
         ("C-c f" . find-name-dired)
         ("C-c F" . find-dired)
         ("C-c C-r" . remember)
         ("M-j" . find-file)
         ("M-J" . find-file-other-window))
  :init
  ;; Preferred way as per documentation
  ;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Backspace-invokes-help.html
  (keyboard-translate ?\C-h ?\C-?)

  ;; Help-map is not a defun and hence does not get mapped
  ;; through the bind mapping above
  (global-set-key (kbd "C-c h") help-map)
  (global-set-key (kbd "C-c h i") 'emacs-index-search)
  (global-set-key (kbd "C-c h I") 'info))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global ",f" 'find-file)
  (key-chord-define-global ",b" 'switch-to-buffer)
  (key-chord-define-global ",c" 'avy-goto-char-timer)
  (key-chord-define-global ",o" 'ace-window)
  (key-chord-define-global ",k" 'kill-buffer-and-window)
  (key-chord-define-global ",v" 'split-window-right)
  (key-chord-define-global ",d" 'delete-other-windows)
  (key-chord-define-global ",s" 'isearch-forward-regexp)
  (key-chord-define-global ",." 'isearch-forward-symbol-at-point)
  (key-chord-define-global ",," 'execute-extended-command))

(provide 'personal-keybindings)
