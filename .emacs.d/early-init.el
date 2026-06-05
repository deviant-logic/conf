;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)

;; necessary to placate errors about gcc from emacs-plus as of recently
(setenv "PATH" "/opt/homebrew/bin:/opt/homebrew/sbin:/Users/alec/bin:/Users/alec/.cabal/bin:/Users/alec/.ghcup/bin:/Users/alec/.cargo/bin:/Users/alec/.rvm/bin:/usr/local/opt/llvm/bin:/usr/local/bin:/Library/TeX/texbin:/usr/bin:/bin:/usr/sbin:/sbin:/Applications/Ghostty.app/Contents/MacOS")
(setq exec-path (split-string (getenv "PATH") path-separator))
