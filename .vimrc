set nocompatible
filetype off

set gcr=a:blinkon0

set sw=2
set ts=2
set expandtab
set ignorecase " required for smartcase
set smartcase

set showcmd

set list lcs=tab:»\ ,trail:·

set wildmenu
set wildmode=longest:full,full

set splitbelow
set splitright

" key bindings

" need to unmap space before it can be <Leader>
nnoremap <SPACE> <Nop>
let mapleader = " "

nnoremap <Leader>w :update<cr>
nnoremap <Leader>q :quit<cr>
nnoremap <Leader>ve :edit ~/.vimrc<cr>
nnoremap <Leader>vs :source ~/.vimrc<cr>

nnoremap <Leader>m :make<cr>

nnoremap <Leader><Leader> V

" begin vundling Here

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-markdown'
Plugin 'tpope/vim-vividchalk'
Plugin 'tpope/vim-commentary'
" Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'dietsche/vim-lastplace'

Plugin 'ervandew/supertab'

Plugin 'godlygeek/tabular'
Plugin 'rodjek/vim-puppet'

Plugin 'kien/ctrlp.vim'

Plugin 'rust-lang/rust.vim'
" Plugin 'vim-syntastic/syntastic'

Plugin 'jiangmiao/auto-pairs'

" Plugin 'sirver/ultisnips'
" Plugin 'honza/vim-snippets'

" let g:UltiSnipsExpandTrigger="<tab>"
" let g:UltiSnipsJumpForwardTrigger="<tab>"
" let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
" let g:UltiSnipsListSnippets="<c-tab>"

Plugin 'derekwyatt/vim-scala'

Plugin 'neovimhaskell/haskell-vim'
Plugin 'enomsg/vim-haskellConcealPlus'

Plugin 'adimit/prolog.vim'

Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
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
