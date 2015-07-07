set nocompatible
set runtimepath=~/.vim,$VIMRUNTIME

if filereadable($HOME . "/.vim/autoload/plug.vim")
        call plug#begin("~/.vim/bundle")
        source ~/.vim/writingdeps.vim
        call plug#end()
endif

if has("win32") || has("gui_macvim")
        set anti guifont=Fantasque\ Sans\ Mono\ Regular:h18
else
        set anti guifont=Fantasque\ Sans\ Mono\ 15
endif

if has("win32")
        autocmd GUIEnter * simalt ~x
endif

set background=light

set wrap
set nonumber
set norelativenumber
set nobackup
set noswapfile
set incsearch
set scrolloff=6
set gcr=a:blinkon0
set guioptions=aei
set mouse=

set spell spelllang=en_gb
nnoremap ]z ]s
nnoremap [z [s

colorscheme pencil

filetype plugin indent on

autocmd VimEnter,BufEnter * Goyo 90x20
autocmd VimEnter * Limelight
autocmd VimEnter * call lexical#init()
autocmd VimEnter * call textobj#sentence#init()
autocmd VimEnter * call textobj#quote#init()
autocmd BufEnter *.md setlocal syntax=markdown | setlocal textwidth=80

cd ~/repos/blog
silent! edit _posts/
