
# antidote logic from https://getantidote.github.io/install
ANTIDOTE=~/.zsh/antidote
PLUGINS=~/.zsh-plugins

zstyle ':antidote:bundle' use-friendly-names 'yes'
zstyle ':antidote:bundle' file $PLUGINS
zstyle ':antidote:static' file $PLUGINS.static.zsh

[[ -e $ANTIDOTE ]] || git clone --depth=1 https://github.com/mattmc3/antidote.git $ANTIDOTE

source $ANTIDOTE/antidote.zsh

antidote load
