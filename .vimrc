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
Bundle 'ervandew/supertab'

colorscheme vividchalk

filetype plugin indent on

