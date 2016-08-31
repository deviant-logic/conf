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

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;; capture

(setq org-default-notes-file (concat org-directory "/capture.org"))

(global-set-key (kbd "C-c c") 'org-capture)

;; TODOs
(setq org-todo-keywords '((type "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red"          :weight bold)
        ("NEXT" :foreground "blue"         :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAIT" :foreground "orange"       :weight bold)))

(setq org-use-fast-todo-selection t)

(setq org-fast-tag-selection-single-key 'expert)

;; tags

(setq org-tag-alist
      '(("@home" . ?h)
        ("@phone" . ?e)))

;; agenda stuff

(setq org-agenda-files (list org-directory))

(setq org-stuck-projects '("-REFILE" ("NEXT") nil ""))
