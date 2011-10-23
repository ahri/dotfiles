set nocompatible                          " not compatible with VI
filetype off

" Call :BundleInstall to use Vundle!
" https://github.com/gmarik/vundle/blob/master/README.md
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" See http://vim-scripts.org
Bundle 'bufexplorer.zip'

" ### coding related
Bundle 'ervandew/supertab'
Bundle 'Syntastic'

" ### git related
Bundle 'tpope/vim-fugitive'
Bundle 'extradite.vim'

filetype plugin indent on " has to be after bundles

nmap <F8> :TagbarToggle<CR>

set ff=unix                               " unix file formats by default
set bs=2                                  " comfortable backspacing
set hidden

" mapping for thinkpad
map <F1> <Esc>
imap <F1> <Esc>

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
colorscheme desert
set background=light
set background=dark
set ruler                                 " it's nice to know where you are in life
set showcmd                               " show command in status line
set incsearch                             " incremental searching - ie. search-as-you-type
set scrolloff=6                           " lines above/below to show for context
set gcr=a:blinkon0                        " stop the cursor blinking in GUI mode
set guioptions=aegirLt                    " set a few gui options
set mouse=                                " disable the mouse when --with-x was specified

if has("gui_running")
  if has("gui_gtk2")
    "set guifont=Inconsolata\ 12
    set guifont=Monospace\ 11
  elseif has("gui_win32")
    set guifont=Consolas:h14:cANSI
  endif
endif

"git settings
set laststatus=2
set statusline+=%{exists('g:loaded_fugitive')?fugitive#statusline():''}
set statusline+=%t       "tail of the filename
set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
set statusline+=%{&ff}] "file format
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%y      "filetype
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file

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

" set some stuff up per filetype
autocmd BufEnter *.py call Tabs(4)
autocmd BufEnter *.rb call Tabs(2)
if has('matchadd')
    autocmd BufEnter *.c,*.php,*.py,*.java call matchadd('TODO', '\(\t\|[\t ]\+$\)')
endif
autocmd BufWrite *.c,*.php,*.py,*.rb,*.java call CleanWhitespace()

" detect vim >= 7.3
if !has('conceal')
    finish
endif
autocmd BufEnter * set colorcolumn=80
