;;; Some utility functions
;;; TODO: Needs cleanup

(defcustom my/light-theme-name 'leuven
  "Name of the light theme.")

(defcustom my/dark-theme-name 'sanityinc-tomorrow-night
  "Name of the dark theme.")

(defun my/setup-light-theme ()
  "Setup a light theme."
  (interactive)
  (disable-theme my/dark-theme-name)
  (load-theme my/light-theme-name))

(defun my/setup-dark-theme ()
  "Setup a dark theme."
  (interactive)
  (disable-theme my/light-theme-name)
  (load-theme my/dark-theme-name)
  (set-background-color "black"))

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun find-file-in-heirarchy (current-dir fname)
  "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR" 
  (let ((file (concat current-dir fname))
        (parent (parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        file
      (when parent
        (find-file-in-heirarchy parent fname)))))

(defun my/yank-dwim (type)
  (let (bounds)
    (if (region-active-p)
        (kill-ring-save (region-beginning) (region-end))
      (progn
        (setq bounds (bounds-of-thing-at-point type))
        (copy-region-as-kill (car bounds) (cdr bounds))))))

(global-set-key (kbd "M-w") '(lambda () (interactive) (my/yank-dwim 'symbol)))
(global-set-key (kbd "M-l") '(lambda () (interactive) (my/yank-dwim 'line)))

;; Setup GTAGSROOT when first called find-tags
(defun my/set-gtags-root()
  (interactive)
  (let (root-path)
    (setq root-path (file-name-directory (find-file-in-heirarchy (buffer-file-name) "GTAGS")))
    (if (string-blank-p root-path)
        ()
      (progn
        (message (concat "Setting GTAGSROOT to " root-path))
        (setenv "GTAGSROOT" root-path)))))

(defadvice find-tag (before my-set-gtags-root)
  "Find the GTAGSROOT if not already set."
  (progn
    (my/set-gtags-root)))
(ad-activate 'find-tag)

(defadvice helm-gtags-dwim (before my-set-gtags-root2)
  (my/set-gtags-root))
(ad-activate 'helm-gtags-dwim)

(defadvice counsel-gtags-dwim (before my-set-gtags-root3)
  (my/set-gtags-root))
(ad-activate 'counsel-gtags-dwim)

;;(defadvice find-tag (after my-set-file-truename)
;;  (setq find-file-visit-truename 't))

(defun my/split-buffer-on-regexp(regexp)
  (interactive "sEnter regexp:")
  (let (buf1 buf2 str1 str2)
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp regexp)
      (setq str1 (buffer-substring-no-properties (point-min) (point)))
      (setq str2 (buffer-substring-no-properties (point) (point-max))))
    (setq buf1 (get-buffer-create (concat (buffer-name) "-buf1")))
    (setq buf2 (get-buffer-create (concat (buffer-name) "-buf2")))
    (with-current-buffer buf1
      (erase-buffer)
      (insert str1))
    (with-current-buffer buf2
      (erase-buffer)
      (insert str2))
    (switch-to-buffer buf1)
    (split-window-sensibly)
    (other-window 1)
    (switch-to-buffer buf2)
    (goto-char (point-min))
    (other-window -1)))

(defun my/text-between-regexp (regexp1 regexp2)
  "Extract out the text between two regexps in to a buffer."
  (interactive "sEnter regexp1:\nsEnter regexp2:")
  (let (buf start-point end-point text)
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp regexp1)
      (setq start-point (point))
      (search-forward-regexp regexp2)
      (setq end-point (point))
      (setq text (buffer-substring-no-properties start-point end-point)))
    (setq buf (get-buffer-create (concat (buffer-name) "-buf1")))
    (with-current-buffer buf
      (erase-buffer)
      (insert text))
    (split-window-sensibly)
    (other-window 1)
    (switch-to-buffer buf)
    (goto-char (point-min))))

(defun my/get-client-from-config()
  "Find .p4config upwards"
  (interactive)
  (let (file ref-path)
    (setq file (find-file-in-heirarchy (buffer-file-name) ".p4config"))
    (if (file-exists-p file)
        (progn 
;          (message "Found file %s" file)
          (find-file file)
          (goto-char 1)
          (search-forward "=")
          (setq ref-path (buffer-substring-no-properties (point) (line-end-position)))
          (message "Your ref-path is %s" ref-path)
          (kill-buffer (current-buffer))
          ref-path) ; return ref-path name
      nil)))

(defun my/set-client()
  "Set P4CLIENT environment variable"
  (interactive)
  (let (ref-path)
    (progn
      (setq ref-path (my/get-client-from-config))
      (setenv "P4CLIENT" ref-path))))

(require 'p4)
(defadvice p4-call-command (before my-set-p4-client())
  (my/set-client))
(ad-activate 'p4-call-command)

(defvar my/dispatch-cmd-name "qsub"
  "Use this command to dispach to grid machines")

(defun my/dispatch-scr-on-grid(scr)
  "Fire the script on the grid"
  (interactive "fEnter the script")
  (let (cmd-name)
    (setq cmd-name (format "%s %s" my/dispatch-cmd-name scr))
    (shell-command cmd-name)))

(defvar my/git-repo-dir "~/.emacs.d/fromgit/"
  "Location where Emacs packages through git are installed.")

(defun my/get-git-repo (url name)
  "Get a git repo from URL and save it at NAME."
  (interactive "sEnter URL: \nsEnter name: ")
  (let* ((full-name (concat my/git-repo-dir  name))
         (cmd (concat "git clone " url " " full-name)))
    (unless (file-exists-p full-name)
      (shell-command cmd))
    (add-to-list 'load-path full-name)))


(defun my/update-git-repo ()
  "Update the installed git repo packages"
  (interactive)
  (let* ((file-list (directory-files my/git-repo-dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))
    (dolist (f file-list)
      (when (file-exists-p (concat f "/.git"))
        (let ((cmd (concat "cd " f "; git pull origin master ; cd -")))
          (message (concat "Updating " f))
          (shell-command cmd))))))

(provide 'functions)
