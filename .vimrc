set nocompatible
filetype off

set sw=2
set ts=2
set expandtab

set list lcs=tab:»\ ,trail:·

set wildmenu
set wildmode=longest:full,full

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-markdown'
Plugin 'tpope/vim-vividchalk'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'

Plugin 'ervandew/supertab'

Plugin 'godlygeek/tabular'
Plugin 'rodjek/vim-puppet'

Plugin 'kien/ctrlp.vim'

Plugin 'sirver/ultisnips'
Plugin 'honza/vim-snippets'

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsListSnippets="<c-tab>"

let g:haskell_conceal_wide = 1

Plugin 'raichoo/haskell-vim'
Plugin 'enomsg/vim-haskellConcealPlus'

Plugin 'bling/vim-airline'
let g:airline_powerline_fonts=0
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
" unicode symbols
let g:airline_left_sep = '»'
let g:airline_right_sep = '«'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.whitespace = 'Ξ'
let g:airline_theme='molokai'

call vundle#end()
filetype plugin indent on

colorscheme vividchalk
