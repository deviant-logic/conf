
# set up antidote; plugins go in ~/.zsh-plugins
source ~/.zsh/init-antidote.zsh

# zle and keybindings
bindkey -e

WORDCHARS='*?_-.[]~=&;!#$%^(){}<>' # effectively, subword mode in zle

autoload -U url-quote-magic
zle -N self-insert url-quote-magic

bindkey '^X^F' fuzzy-search-and-edit

# history

export HISTSIZE=1000000
export SAVEHIST=1000000
export HISTFILE=~/.history

setopt append_history
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_reduce_blanks

# completion

if whence brew &> /dev/null; then
  # assume other stuff is more right than brew-installed stuff
  fpath+=$(brew --prefix)/share/zsh/site-functions
fi

if whence nix &> /dev/null; then
  # assume nix-installed stuff is more right than other stuff
  fpath=( ~/.nix-profile/share/zsh/site-functions $fpath )
fi

autoload -Uz compinit
compinit

# miscellaneous other options and things

setopt auto_cd
setopt multios
setopt extended_glob
setopt interactive_comments

export PAGER=less
export EDITOR=vi
alias vi=vim
alias ls='ls -GF'

unalias run-help # https://zsh.sourceforge.io/Doc/Release/User-Contributions.html#index-run_002dhelp_002c-use-of
autoload run-help


# direnv and starship
eval "$(direnv hook zsh)"
eval "$(starship init zsh)" # prompt magic is now in ~/.config/starship.toml
