
(require 'paredit)

(eval-after-load 'diminish
  '(diminish 'paredit-mode " ()"))

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
