
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

        ns-auto-hide-menu-bar t

        ns-use-native-fullscreen nil))

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

(when (fboundp 'winner-mode)
  (winner-mode 1))

; (add-hook 'el-get-package-menu-mode-hook 'hl-line-mode)

; gc

(setq gc-cons-threshold 20000000)

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

(global-set-key (kbd "C-c .")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs")))

(global-set-key (kbd "<C-tab>")
                'completion-at-point)

(global-set-key (kbd "<M-return>")
                'toggle-frame-fullscreen)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(transient-mark-mode -1)

; read-only

(setq kill-read-only-ok t
      view-read-only    t)

; whitespace

(setq-default fill-column 100)

(setq whitespace-style
      '(face tabs trailing lines-tail
             space-before-tab newline
             indentation space-after-tab
             tab-mark))

;; setting nil uses `fill-column'
(setq whitespace-line-column nil)

(global-whitespace-mode 1)
(setq whitespace-global-modes t)

;; org-mode

(with-eval-after-load 'org
  (load (concat (file-name-as-directory user-emacs-directory) "org.el")))

;; packages

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  ; :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; (use-package color-theme-modern
;;   :config
;;   (load-theme 'charcoal-black t t)
;;   (enable-theme 'charcoal-black))

(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package delight)

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  :delight (paredit-mode " ()"))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package ws-butler
  :delight
  :config
  (ws-butler-global-mode t))

(use-package emacs
  :delight
  (emacs-lisp-mode "Elisp" :major)
  (global-whitespace-mode nil))

(use-package flx)

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t
        ido-use-faces            nil))

(use-package ido-completing-read+)

(use-package smex
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))

(use-package amx
  :config
  (amx-mode 1))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package ace-window
  :bind
  ("C-c o" . ace-window)
  :config
  (setq aw-keys '(?a ?e ?u ?h ?t ?s)))

(use-package dumb-jump)

(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (setq magit-repo-dirs          '("~/wip" "~/src")
        magit-push-always-verify nil))

;; (use-package magithub
;;   :after magit
;;   :config (magithub-feature-autoinject t))

(use-package yaml-mode)

(use-package mode-local)

(use-package haskell-mode
  :config
  ;; (custom-set-variables '(haskell-process-type 'stack-ghci))
  (custom-set-variables '(haskell-process-args-stack-ghci
                          '("--ghci-options=-ferror-spans -fshow-loaded-modules" "--no-build" "--no-load")))
  (custom-set-variables '(haskell-process-type 'cabal-new-repl))
  (custom-set-variables '(haskell-font-lock-symbols t))
  (custom-set-variables '(haskell-indent-offset 2))
  (remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook    'haskell-mode-hook 'haskell-hook)

  (defun haskell-hook ()
    (turn-on-haskell-indent)
    (interactive-haskell-mode)
    ;; (intero-mode)
    (when (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))
    (set (make-local-variable 'whitespace-line-column) 200))

  :bind (:map haskell-mode-map
         ("C-c C-l" . haskell-process-load-file)
         ("C-`"     . haskell-interactive-bring)
         ("C-c c"   . haskell-process-cabal)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-f" . haskell-cabal-visit-file)
         ("M-."     . haskell-mode-jump-to-def-or-tag)
        :map haskell-cabal-mode-map
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c c"   . haskell-process-cabal)
         ("C-`"     . haskell-interactive-bring)
         ("C-c C-z" . haskell-interactive-switch)))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package ialign
  :bind
  ("C-x l" . ialign))

(use-package weechat)

(use-package try)

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys))

(use-package focus)

(load custom-file 'noerror) ; setting custom-file does nothing otherwise

;;; some packages to look into

;; https://github.com/emacscollective/no-littering
;; https://github.com/magnars/expand-region.el
;; https://github.com/victorhge/iedit
;; https://github.com/cyrus-and/zoom
;; https://github.com/wasamasa/eyebrowse
;; https://github.com/abo-abo/hydra
;; https://github.com/syohex/emacs-git-messenger
;; https://github.com/Wilfred/deadgrep
;; https://github.com/fgeller/highlight-thing.el
;; https://github.com/soutaro/hungry-delete.el
;; https://github.com/rejeep/drag-stuff.el

;; https://github.com/magnars/change-inner.el
;; https://github.com/ganmacs/emacs-surround
;; https://github.com/nivekuil/corral
