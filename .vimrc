set nocompatible              " be iMproved, required
filetype off                  " required

" $ mkdir -p ~/.vim/bundle && git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
Plugin 'gmarik/Vundle.vim'

" See http://vim-scripts.org
" See https://github.com/vim-scripts/
Plugin 'bufexplorer.zip'
" Plugin 'ctrlp.vim'
" map <leader>y :CtrlPBuffer<cr>
Plugin 'bling/vim-airline'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
set encoding=utf-8
set t_Co=256
set laststatus=2

" ### coding related
Plugin 'markdown'
Plugin 'html5.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'jelera/vim-javascript-syntax'
" completion
Plugin 'Valloric/YouCompleteMe'
Plugin 'marijnh/tern_for_vim'
let g:tern_map_keys=1
let g:tern_show_argument_hints='on_hold'
let g:tern#command = ["/home/adam/iojs-v1.3.0-linux-x64/bin/node", '/home/adam/.vim/bundle/tern_for_vim/node_modules/tern/bin/tern', '--no-port-file']

" run linters etc.
Plugin 'scrooloose/syntastic'
" use \\\ to comment stuff
Plugin 'commentary.vim'
" close quotes etc.
Plugin 'Raimondi/delimitMate'
" help to see indenting
Plugin 'nathanaelkane/vim-indent-guides'

" ### git related
Plugin 'mhinz/vim-signify'

" ### colorscheme
Plugin 'tomasr/molokai'

call vundle#end()
filetype plugin indent on " has to be after bundles

set ff=unix                               " unix file formats by default
set bs=2                                  " comfortable backspacing
set backspace=indent,eol,start
set hidden
set smartcase                             " ignore case in searches unless there's a capital in the search
set ignorecase

" anti-tab settings
function! Tabs(spaces)
        " number of spaces tab counts as
        exec "set softtabstop=" . a:spaces
        " auto-indent shift
        exec "set shiftwidth=" . a:spaces
        " how many spaces do tabs look like?
        exec "set tabstop=" . a:spaces
        " expand a tab into spaces
        set expandtab
endfunction

" default to 4 spaces for a tab
call Tabs(4)

" clean up the whitespace in a file, as long as it's not switched off with:
" :let g:unclean=1
function! CleanWhitespace()
        if g:dirty == 0
                retab
                %s/ \+$//e
        endif
endfunction
let g:dirty = 0

function! Dirty()
        let g:dirty = 1
endfunction

" backup/recovery related
set nobackup
set noswapfile

" coding oriented settings
set ai                                    " auto indent
set tw=0                                  " text-width, set to zero - I don't want line breaks leaking in
set nowrap                                " i just don't like it wrapping
set nu                                    " number lines
set nocuc nocul                           " vt/hz line highlight -- I'm always losing my place
autocmd BufEnter * :syntax sync fromstart " don't be clever about syntax, just parse the whole file
"syn sync minlines=500                     " look back 500 lines to figure out syntax (may be better than above if slowdown occurs)

" 'look' oriented settings
set background=light
set background=dark
set ruler                                 " it's nice to know where you are in life
set showcmd                               " show command in status line
set incsearch                             " incremental searching - ie. search-as-you-type
set scrolloff=6                           " lines above/below to show for context
set gcr=a:blinkon0                        " stop the cursor blinking in GUI mode
set guioptions=aegirLt                    " set a few gui options
set mouse=                                " disable the mouse when --with-x was specified

silent! colorscheme molokai

if has("gui_running")
  if has("win32")
    set anti guifont=Consolas_for_Powerline_FixedD:h11:cANSI
    let g:airline_symbols = {}
    let g:airline_left_sep = "\u2b80"
    let airline#extensions#tabline#left_sep = "\u2b80"
    let g:airline_left_alt_sep = "\u2b81"
    let airline#extensions#tabline#left_alt_sep = "\u2b81"
    let g:airline_right_sep = "\u2b82"
    let g:airline_right_alt_sep = "\u2b83"
    let g:airline_symbols.branch = "\u2b60"
    let g:airline_symbols.readonly = "\u2b64"
    let g:airline_symbols.linenr = "\u2b61"
    let g:airline_symbols.whitespace = "\u2736"
  elseif has("macvim")
    set anti guifont=*
  else
    " set anti guifont=Monospace\ 11
    " set anti guifont=Consolas\ for\ Powerline\ 11
    set anti guifont=Monaco\ for\ Powerline\ 11
    " set anti guifont=monofur\ for\ Powerline\ 12
  endif
else
  let g:airline_powerline_fonts = 0
  set t_Co=16
  set encoding=ansi
endif

"git settings
set laststatus=2
" set statusline=
" set statusline+=%{exists('g:loaded_fugitive')?fugitive#statusline():''}
" set statusline+=%t       "tail of the filename
" set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
" set statusline+=%{&ff}] "file format
" set statusline+=%h      "help file flag
" set statusline+=%m      "modified flag
" set statusline+=%r      "read only flag
" set statusline+=%y      "filetype
" set statusline+=%=      "left/right separator
" set statusline+=%c,     "cursor column
" set statusline+=%l/%L   "cursor line/total lines
" set statusline+=\ %P    "percent through file

" misc non-settings
syntax on
" map  :w!<CR>:!aspell check %<CR>:e! %<CR>
" map <F2> <Esc>:1,$!xmllint --format -<CR>
filetype on
filetype plugin on
let g:bufExplorerShowRelativePath=1  " Show relative paths.

" avoid having to press ESC
"imap jj <ESC>
"imap JJ <ESC>

" allow fingers to stay on the home row
"nnoremap j h
"nnoremap k j
"nnoremap l k
"nnoremap ; l

noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
inoremap <Right> <NOP>

" Don't use ex mode
noremap Q <NOP>

" set some stuff up per filetype
autocmd BufEnter *.py call Tabs(4)
autocmd BufEnter *.rb call Tabs(2)
autocmd BufEnter *.js call Tabs(2)
autocmd BufEnter Rakefile set syntax=ruby | call Tabs(2)
autocmd BufEnter Buildfile set syntax=ruby | call Tabs(2)
autocmd BufEnter build.gradle set syntax=groovy
if has('matchadd')
    autocmd BufEnter *.c,*.php,*.py,*.java call matchadd('TODO', '\(\t\|[\t ]\+$\)')
endif
autocmd BufWrite *.c,*.php,*.py,*.rb,*.java call CleanWhitespace()

" detect vim >= 7.3
if !has('conceal')
    finish
endif
autocmd BufEnter * set colorcolumn=80
