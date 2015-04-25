
(add-hook 'haskell-mode-hook 'haskell-hook)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

(custom-set-variables '(haskell-process-type 'cabal-repl)
                      '(haskell-font-lock-symbols t))

(defun haskell-hook ()
  (turn-on-haskell-indent)
  ; (turn-on-haskell-font-lock)
  ; (turn-on-haskell-decl-scan)
  (interactive-haskell-mode)
  ; (define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
  ; (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)

  ;; Switch to the REPL.
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  ;; Build the Cabal project.
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; Interactively choose the Cabal command to run.
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  ; (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; Switch to the appropriate cabal file

  (define-key haskell-mode-map (kbd "C-c C-f") 'haskell-cabal-visit-file)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  ; (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  ; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
  ;; Same as above but backwards.
  (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)

  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode -1)))

(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))
