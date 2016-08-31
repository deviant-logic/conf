
##### options #####
setopt prompt_subst
setopt auto_pushd

##### terminal magic #####

autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
  colors
fi

function prompt_char {
  [ -x "`which git`" ] && git branch >/dev/null 2>/dev/null && echo '\u00b1 ' && return
  [ -x "`which hg`"  ] && hg root >/dev/null 2>/dev/null && echo '\u263f ' && return
  [ -x "`which darcs`"  ] && darcs show tags >/dev/null 2>/dev/null && echo '\u274a ' && return
}

##### prompt #####
PROMPT='%m%# '
RPROMPT='%(?..%{$fg[red]%}%B[%?]%b%{$reset_color%} )$(prompt_char):%~'

##### aliases #####
alias ls='ls -GF'
alias ocaml='ledit -h ~/.ocaml_history ocaml'
if [ -x `which vim` ]; then
  alias vi=vim
fi
alias R='R --no-save'

##### functions #####
typeset -U fpath
fpath=(/usr/local/share/zsh-completions $fpath)
fpath=(/usr/local/share/zsh/site-functions $fpath)
fpath=($HOME/.zsh-functions $fpath)

##### keybindings #####
bindkey -e

##### completion #####
autoload -U compinit
compinit

compdef conf=git # use git completion for my conf script

##### history #####

HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.history

setopt append_history
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_reduce_blanks

##### zsh voodoo #####

setopt extended_glob
setopt multios

autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

autoload insert-files
zle -N insert-files
bindkey '^X^F' insert-files

syntax_highlighting=/usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[[ -r $syntax_highlighting ]] && source $syntax_highlighting

##### interactivity #####

setopt interactive_comments
export PAGER=less
if [ -x `which vim` ]; then
  export EDITOR=vim
else
  export EDITOR=vi
fi
export REPORTTIME=2

stty dsusp undef # let C-y work in ghci

##### but if I'm in emacs... #####

if [[ $EMACS = t ]]; then
    RPROMPT=""
    export PAGER=cat
fi

##### iTerm2 integration #####

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
