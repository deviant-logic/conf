;; stuff that should work when the rest of my .emacs is broken

(blink-cursor-mode -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(setf ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq disabled-command-function nil)
(repeat-mode 1)

(setq-default indent-tabs-mode nil)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory)))
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq confirm-nonexistent-file-or-buffer nil
      vc-follow-symlinks                 t)

(remove-hook 'kill-buffer-query-functions
             'process-kill-buffer-query-function)

(when (eq system-type 'darwin)
  (setq mac-option-modifier  'hyper
        mac-option-modifier  'super
        mac-command-modifier 'meta

        ;; ns-auto-hide-menu-bar t
        ns-use-native-fullscreen nil))

(setq display-time-day-and-date t
      display-time-24hr-format  t
      battery-mode-line-format  " [%b%t]")

(display-time-mode    1)
(display-battery-mode 1)

(which-function-mode 1)

(electric-pair-mode 1)

(global-set-key (kbd "C-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key (kbd "C-c .")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs")))

(global-set-key (kbd "C-<tab>") 'completion-at-point)
(global-set-key (kbd "M-<return>") 'toggle-frame-fullscreen)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(transient-mark-mode -1)

;; read-only buffers

(setq kill-read-only-ok t
      view-read-only    t)

;; fido/completion

(fido-mode t)
(setq completion-styles '(basic partial-completion initials))


;; use straight.el

(setq straight-use-package-by-default t)
(load (expand-file-name "init-straight.el" user-emacs-directory))
(straight-use-package 'use-package)

(use-package delight)

;; emacs builtins (TODO: make a `use-builtin' macro)

(use-package compile
  :straight (:type built-in)
  :config
  (setq compilation-scroll-output t)
  :bind ("C-c m" . compile))

(use-package eldoc
  :straight (:type built-in)
  :delight)

(use-package ibuffer
  :straight (:type built-in)
  :bind ("C-x C-b" . ibuffer))

(use-package uniquify
  :straight (:type built-in)
  :config
  (setq uniquify-buffer-name-style 'reverse))

(use-package whitespace
  :straight (:type built-in)
  :delight whitespace-mode
  :config
  (setq whitespace-style
        '(face tabs trailing lines-tail
               space-before-tab newline
               space-after-tab
               tab-mark))
  (setq whitespace-line-column nil)

  (global-whitespace-mode 1))

;; not-built-ins

(use-package ace-window
  :config
  (setq aw-keys '(?a ?e ?i ?d ?h ?t ?\;))
  :bind
  ("C-c o" . ace-window))

(use-package avy
  :config
  (avy-setup-default)
  (setq avy-keys (number-sequence ?a ?z))
  (dolist (c avy-dispatch-alist)
    (delq (car c) avy-keys))
  :bind
  ("C-'" . avy-goto-char-timer)
  ("M-g M-g" . avy-goto-line)
  ("C-c C-'" . avy-resume))

(use-package change-inner
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer))

(use-package default-text-scale
  :config
  (default-text-scale-mode))

(use-package dirvish
  :config
  (dirvish-override-dired-mode))

(use-package dumb-jump)

(use-package eat
  ;; have to run `eat-compile-terminfo' manually on install.
  :straight (:type git
             :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  :bind
  ("C-c j"   . eat)
  ("C-c C-j" . eat-project)
  (:repeat-map ah/eat-repeat-map
    ("p" . eat-previous-shell-prompt)
    ("n" . eat-next-shell-prompt)))

(use-package envrc
  :config
  (setq envrc-none-lighter nil)
  (envrc-global-mode))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package git-link
  :bind
  ("C-c C-g l" . git-link))

(use-package haskell-mode
  :config

  (remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  (defun haskell-hook ()
    (turn-on-haskell-indent)
    (interactive-haskell-mode)
    (when (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))
    (set (make-local-variable 'whitespace-line-column) 100))


  :custom
  (haskell-process-type 'auto)
  (haskell-font-lock-symbols t)
  (haskell-indent-offset 2)
  (haskell-process-args-stack-ghci
   '("--ghci-options=-ferror-spans -fshow-loaded-modules" "--no-build" "--no-load"))

  :hook (haskell-mode . haskell-hook)

  :bind (:map haskell-mode-map
         ("C-c C-l" . haskell-process-load-file)
         ("C-`"     . haskell-interactive-bring)
         ("C-c c"   . haskell-process-cabal)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-f" . haskell-cabal-visit-file)
         ("M-."     . haskell-mode-jump-to-def)
        :map haskell-cabal-mode-map
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c c"   . haskell-process-cabal)
         ("C-`"     . haskell-interactive-bring)
         ("C-c C-z" . haskell-interactive-switch)))

(use-package helpful
  :bind
  ("C-h f"   . helpful-callable)
  ("C-h v"   . helpful-variable)
  ("C-h k"   . helpful-key)
  ("C-c C-d" . helpful-at-point))

(use-package ialign
  :config
  (setq ialign-pcre-mode t)
  (setq ialign-initial-regexp "(\\s+)")
  :bind
  ("C-x l" . ialign))

(use-package magit
  :bind ("C-c g" . magit-status)
  :config
  (setq magit-repo-dirs          '("~/wip" "~/src")
        magit-push-always-verify nil))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines))

;; mwim does a better job of my old toggle-bol/bti function.
(use-package mwim
  :config
  ;; moved `mwim-line-beginning' in front to mimic the old bol/bti
  (setq mwim-beginning-position-functions
        '(mwim-line-beginning
          mwim-block-beginning
          mwim-code-beginning
          mwim-comment-beginning))

  :bind
  ([remap move-beginning-of-line] . mwim-beginning)
  ([remap move-end-of-line] . mwim-end))

(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package pcre2el)

(use-package prescient
  :config
  (add-to-list 'completion-styles 'prescient))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens ; https://ebzzry.com/en/emacs-pairs/
  :delight
  :custom
  (sp-show-pair-from-inside nil)
  (sp-base-key-bindings 'paredit)
  :hook
  (prog-mode . smartparens-strict-mode))

(use-package smartscan
  :hook
  (prog-mode . smartscan-mode))

(use-package string-inflection
  :bind
  ("C-c `" . string-inflection-all-cycle))

(use-package visual-regexp-steroids
  :config
  (setq vr/engine 'pcre2el)
  :bind
  ("M-%"         . 'vr/replace)
  ("C-S-c C-S-m" . 'vr/mc-mark)
  ("C-M-r"       . 'vr/isearch-backward)
  ("C-M-s"       . 'vr/isearch-forward))

(use-package which-key
  :delight
  :config
  (which-key-mode))

(use-package ws-butler
  :delight
  :config
  (ws-butler-global-mode t))

(use-package yaml-mode)


;; this should be last

(load custom-file 'noerror)
