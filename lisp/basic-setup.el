;;; Changing some basic Emacs defaults

(use-package emacs
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)

  (setq inhibit-startup-message t)
  (setq initial-scratch-message "")

  (setq custom-file (concat user-emacs-directory "/custom.el"))
  (load custom-file 'noerror)

  ;; Keep all backup and auto-save files in one directory
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  ;; UTF-8 please
  (setq locale-coding-system 'utf-8)   ; pretty
  (set-terminal-coding-system 'utf-8)  ; pretty
  (set-keyboard-coding-system 'utf-8)  ; pretty
  (set-selection-coding-system 'utf-8) ; please
  (prefer-coding-system 'utf-8)        ; with sugar on top

  ;; Answering just 'y' or 'n' will do
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Turn off the blinking cursor
  (blink-cursor-mode -1)

  (setq-default indent-tabs-mode nil)
  (setq-default indicate-empty-lines t)

  ;; delete the region when typing, just like as we expect nowadays.
  (delete-selection-mode t)

  ;; show time
  (display-time)

  ;; winner mode to undo/redo window configurations
  (winner-mode 1)

  ;; always show the function
  (which-function-mode 1)

  ;; no bells please
  (defun my-bell-func()) ;; empty
  (setq ring-bell-function 'my-bell-func)
  (setq visible-bell nil)

  ;; http://endlessparentheses.com/faster-pop-to-mark-command.html
  ;; Go up last positions using C-u C-SPC C-SPC
  ;; instead of C-u C-SPC C-u C-SPC
  (setq set-mark-command-repeat-pop t)

  ;; If same marks are saved in the ring, pop out the same ones
  (defun my/multi-pop-to-mark (orig-fun &rest args)
    "Call ORIG-FUN until the cursor moves.
  Try the repeated popping up to 10 times."
    (let ((p (point)))
      (dotimes (i 10)
        (when (= p (point))
          (apply orig-fun args)))))

  (advice-add 'pop-to-mark-command :around
              #'my/multi-pop-to-mark)

  ;; Open in maximized frame
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(provide 'basic-setup)
