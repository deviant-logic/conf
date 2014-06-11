(require 'org)

;; aesthetics
(setq org-hide-leading-stars     t
      org-cycle-separator-lines  0
      org-blank-before-new-entry '((heading)
                                   (plain-list-item . auto))
      org-src-fontify-natively   t)

;; magic org keys
(setq org-special-ctrl-a/e    t
      org-special-ctrl-k      t
      org-use-speed-commands  t
      org-return-follows-link t)

(global-set-key (kbd "C-c l") 'org-store-link)

;; capture

(setq org-default-notes-file (concat org-directory "/capture.org"))

(global-set-key (kbd "C-c c") 'org-capture)
