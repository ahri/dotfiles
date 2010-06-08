set nocompatible                          " not compatible with VI
set ff=unix                               " unix file formats
set bs=2                                  " comfortable backspacing

" anti-tab settings
set softtabstop=8                         " number of spaces tab counts as
set shiftwidth=8                          " auto-indent shift
set tabstop=8                             " how many spaces do tabs look like?
set expandtab                             " expand a tab into spaces

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
" select a font; last one wins!
set guifont=Courier\ New\ 11
set guifont=Envy\ Code\ R\ 11
" and for windows...
"set guifont=Courier_New:h10:cANSI
"set guifont=Envy_Code_R:h10

"git settings
set laststatus=2
set statusline=<%{GitBranch()}>\ 
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
map  :w!<CR>:!aspell check %<CR>:e! %<CR>
map <F2> <Esc>:1,$!xmllint --format -<CR>
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

autocmd BufWrite *.c,*.php,*.py %s/\t/        /e | %s/ \+$//e
