

(eval-after-load 'weechat
  '(progn (require 'weechat-tracking)
          (set-variable 'weechat-tracking-types '(:highlight :message))))

(defun weechat-hook ()
  (interactive)
  (whitespace-mode -1))

(add-hook 'weechat-mode-hook 'weechat-hook)

(load "~/.weechat-password")

