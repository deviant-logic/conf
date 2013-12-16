
;; stuff that comes with emacs
; stuff that should always happen should come first

; interface

(blink-cursor-mode -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(setf ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq disabled-command-function nil)

(setq-default indent-tabs-mode nil)

(setq backup-directory-alist
      `(("." . ,(concat (file-name-as-directory user-emacs-directory)
                        "backup")))
      custom-file (concat (file-name-as-directory user-emacs-directory)
                          "custom.el"))

(setq confirm-nonexistent-file-or-buffer nil)
(remove-hook 'kill-buffer-query-functions 'process-kill-buffer-query-function)

(ido-mode t)
(setq ido-use-filename-at-point 'guess
      ido-use-url-at-point      t
      ido-create-new-buffer     'always)

(when (eq system-type 'darwin)
  (setq mac-option-modifier  'hyper
        mac-option-modifier  'super
        mac-command-modifier 'meta

        ns-auto-hide-menu-bar t))

(when (eq system-type 'gnu/linux)
  (menu-bar-mode -1))

(setq display-time-day-and-date t
      display-time-24hr-format  t
      battery-mode-line-format " [%b%t]")

(display-time-mode 1)
(display-battery-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

(which-function-mode)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(setq eldoc-minor-mode-string nil)

; parentheses

(setq show-paren-style 'expression)
(show-paren-mode)

(electric-pair-mode)

; keybindings

(global-set-key (kbd "C-x C-b") 'ibuffer)
(defun toggle-bol/bti ()
  (interactive)
  (if (looking-at "^")
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key (kbd "C-a") 'toggle-bol/bti)

(global-set-key (kbd "C-j")
                '(lambda ()
                   (interactive)
                   (join-line -1)))

(global-set-key (kbd "C-c m") 'compile)

(global-set-key (kbd "C-c j") 'shell)
(global-set-key (kbd "C-c g") 'magit-status)

(global-set-key (kbd "C-c .")
                (lambda () 
                  (interactive)
                  (find-file "~/.emacs")))

(global-set-key (kbd "<C-tab>")
                'completion-at-point)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(transient-mark-mode -1)
(setq kill-read-only-ok t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-user-package-directory
      "~/.emacs.d/init-package")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq el-get-sources
      `((:name haskell-mode
	       :type elpa
	       :repo ("marmalade" . "http://marmalade-repo.org/packages/"))))

(setq el-get-packages
      `(git-gutter-fringe
	magit
	paredit
	diminish
	rainbow-delimiters
	smex
	solarized-theme
	twilight-anti-bright-theme
	exec-path-from-shell))

(el-get 'sync
	(append el-get-packages
		(mapcar 'el-get-source-name el-get-sources)))

; (load-theme 'twilight-anti-bright t)
(load-theme 'solarized-dark t)
