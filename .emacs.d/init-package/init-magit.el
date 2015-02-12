
(setq magit-repo-dirs '("~/wip" "~/src"))

(defun diminish-mrev ()
  (diminish 'magit-auto-revert-mode))
(eval-after-load 'diminish
  '(add-hook 'magit-auto-revert-mode-hook
             'diminish-mrev))
