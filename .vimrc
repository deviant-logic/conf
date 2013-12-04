set nocompatible
filetype off

set sw=2

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'tpope/vim-sensible'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-vividchalk'
Bundle 'tpope/vim-commentary'

Bundle 'ervandew/supertab'

Bundle 'godlygeek/tabular'
Bundle 'rodjek/vim-puppet'

colorscheme vividchalk

filetype plugin indent on

