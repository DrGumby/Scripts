" init.vim
" Personal neovim configuration file
" Author: Kamil Vojanec <xvojan00@stud.fit.vutbr.cz>

" Firstly, tell neovim not to behave like Vi
set nocompatible

" Set 256 colour mode for neovim
set t_Co=256
set t_ut=

" Use 4 spaces by default
set tabstop=4 softtabstop=4
set expandtab
set smartindent

" Do not wrap lines when they are too long
set nowrap

" Make backspace work like most other programs
set backspace=2

" Automatically reload files changed outside neovim
set autoread

" Allow mouse selection
set ttyfast
set mouse=a

" Case insensitive search
set ignorecase

" Open new splits to the right or bottom
set splitright

" Show line numbers
set number

" C-s to save in any mode
nnoremap <C-s> :w<cr>
inoremap <C-s> <Esc>:w <cr>
vnoremap <C-s> <Esc>:w <cr>

" Use space as leader key
let mapleader=" "

" Netrw parameters
let g:netrw_browse_split=4
let g:netrw_banner=0
let g:netwr_altv=1
let g:netrw_winsize=25
let g:netrw_liststyle=3

" Leader key mappings
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nnoremap <leader>u :UndotreeShow<CR>
nnoremap <leader>pv :Vex <bar> :vertical resize 30<CR>
nnoremap <silent> <Leader>+ :vertical resize +5<CR>
nnoremap <silent> <Leader>- :vertical resize -5<CR>

" Set shorter update time
set updatetime=300

" We do not need swp files everywhere
set noswapfile
set nobackup

" Enable syntax highlight
syntax on

filetype plugin indent on

call plug#begin(stdpath('data').'/plugged')
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'dense-analysis/ale'
Plug 'vim-scripts/DoxygenToolkit.vim'
Plug 'lyuts/vim-rtags'
Plug 'ycm-core/YouCompleteMe'
Plug 'mbbill/undotree'
call plug#end()

" Youcompleteme settings
nnoremap <silent> <Leader>gd :YcmCompleter GoTo<CR>
nnoremap <silent> <Leader>gf :YcmCompleter FixIt<CR>

" Use powerline for airline
let g:airline_powerline_fonts=1

" Use Dark+ colorscheme from vscode
colorscheme gruvbox
set background=dark
let g:airline_theme = 'gruvbox'

" C/C++ additional syntax highlighting
let g:cpp_class_scope_highlight=1
let g:cpp_member_variable_highlight=1
let g:cpp_class_decl_highlight=1
let g:cpp_posix_standard=1

" Use ALE in statusline
let g:airline#extensions#ale#enabled=1

" Trims trailing whitespace
function TrimWhiteSpace()
  %s/\s*$//
  ''
endfunction

" Trim whitespace on buffer save
autocmd BufWritePre * call TrimWhiteSpace()

" Disable Arrow keys in Normal mode
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" Disable Arrow keys in Insert mode
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>
