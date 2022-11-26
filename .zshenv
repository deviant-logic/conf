unsetopt GLOBAL_RCS
export PATH=$HOME/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$HOME/.rvm/bin:/usr/local/opt/llvm/bin:/usr/local/bin:/Library/TeX/texbin:$PATH

if [[ -e ~/.nix-profile/etc/profile.d/nix.sh ]]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
