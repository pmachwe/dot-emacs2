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

(use-package f)

(use-package shell
  :config

  (defun shell-same-window-advice (orig-fn &optional buffer)
    "Advice to make `shell' reuse the current window. Intended as :around advice."
    (let* ((buffer-regexp
            (regexp-quote
             (cond ((bufferp buffer)  (buffer-name buffer))
                   ((stringp buffer)  buffer)
                   (:else             "*shell*"))))
           (display-buffer-alist
            (cons `(,buffer-regexp display-buffer-same-window)
                  display-buffer-alist)))
      (funcall orig-fn buffer)))

  (advice-add 'shell :around #'shell-same-window-advice)

  (defun pm/shell (&optional name)
    "Open a new shell everytime with unique buffer-name.
With universal arg, provide a name which will be made unique."
    (interactive
     (if current-prefix-arg
         (list (read-string "Shell name: "))
       (list "shell")))
    ;; -i gets alias definitions from .bash_profile
    (setq-local shell-command-switch "-ic")
    (shell (generate-new-buffer-name name)))

  ;; Use Git Bash as shell on Windows.
  (defvar win-git-path "C:/Program Files/Git"
    "Git executable path on Windows.")
  (require 'f)
  (when (and (equal system-type 'windows-nt)
             (file-exists-p win-git-path))
    (setq explicit-shell-file-name
          (f-join win-git-path "bin/bash.exe"))
    (setq shell-file-name explicit-shell-file-name)
    (add-to-list 'exec-path (f-join win-git-path "bin")))

  (defun pm/shell-command-on-current-file (cmd)
    "Shell script/command CMD on current file."
    (interactive "sCommand: ")
    (shell-command
     (format "%s %s" cmd (shell-quote-argument (buffer-file-name)))))

  :bind (("<f5>" . pm/shell)
         ("C-! C-!" . pm/shell-command-on-current-file)
         :map shell-mode-map
         ("C-j" . comint-send-input)))

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-isearch-filenames 'dwim)
  (setq delete-by-moving-to-trash t)
  ;(setq dired-listing-switches "-altr --group-directories-first")
  (setq dired-dwim-target t)
  :hook (dired-mode . hl-line-mode))

(use-package dired-async
  :after (dired async)
  :config
  (dired-async-mode 1))

(use-package wdired
  :after dired
  :commands (wdired-mode
             wdired-change-to-wdired-mode)
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-tab>" . dired-subtree-remove)))

(use-package isearch
  :config
  (setq search-whitespace-regexp ".*")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace t)
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode)
  :config
  (setq electric-pair-pairs
        '(
          (?\" . ?\")
          (?\{ . ?\}))))

(use-package replace
  :config

  (eval-when-compile
    (require 'cl))

  (defun get-buffers-matching-mode (mode)
    "Returns a list of buffers where their major-mode is equal to MODE"
    (let ((buffer-mode-matches '()))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (if (eq mode major-mode)
              (add-to-list 'buffer-mode-matches buf))))
      buffer-mode-matches))

  (defun multi-occur-in-this-mode ()
    "Show all lines matching REGEXP in buffers with this major mode."
    (interactive)
    (multi-occur
     (get-buffers-matching-mode major-mode)
     (car (occur-read-primary-args))))

  :bind ("M-s O" . multi-occur-in-this-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer-other-window)
  :config
  (setq ibuffer-saved-filter-groups
        (quote (("mygroups"
                 ("dired" (mode . dired-mode))
                 ("perl" (mode . cperl-mode))
                 ("erc" (mode . erc-mode))
                 ("planner" (or
                             (name . "^\\*Calendar\\*$")
                             (name . "^diary$")
                             (mode . muse-mode)))
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))
                 ("gnus" (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . mail-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))))))
  (setq ibuffer-expert t)
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode 1)
               (ibuffer-switch-to-saved-filter-groups "mygroups"))))

(use-package windmove
  :bind (("C-x <up>" . windmove-up)
         ("C-x <down>" . windmove-down)
         ("C-x <left>" . windmove-left)
         ("C-x <right>" . windmove-right)))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :init
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package emacs
  :if (or (string-equal system-type "windows-nt")
          (string-equal system-type "ms-dos"))
  :config
  (setq default-directory (getenv "HOME"))
  (setq w32-get-true-file-attributes nil)
  (message "Setting up options for %s" system-type)
  (remove-hook 'find-file-hooks 'vc-find-file-hook))

(use-package imenu
  :config

  (defun pm/use-pacakge-imenu ()
    "Setup FLOW steps for imenu."
    (interactive)
    (add-to-list 'imenu-generic-expression '(nil "use-package.*" 0)))

  (add-hook 'emacs-lisp-mode-hook 'pm/use-pacakge-imenu))

(provide 'basic-setup)
