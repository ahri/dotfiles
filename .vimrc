" The following are for use with vim-update-bundles
" ### colorschemes
" Bundle: peaksea
" Bundle: tpope/vim-vividchalk
"
" ### random vim stuff
" Bundle: tpope/vim-speeddating
" Bundle: tpope/vim-surround
" # :%Subvert/facilit{y,ies}/building{,s}/g (or just :%S)
" Bundle: tpope/vim-abolish
" Bundle: tpope/vim-repeat
" Bundle: scrooloose/nerdtree
"
" ### coding related
" # insert-mode completions with tab
" Bundle: ervandew/supertab
" # :TlistToggle :help taglist-commands
" Bundle: taglist-plus
" # TextMate style tab-code-gen
" Bundle: snipMate
"
" ### git related
" Bundle: tpope/vim-fugitive
" Bundle: extradite.vim
"
" ### python related
" #Bundle: ehamberg/vim-cute-python
" #Bundle: python_calltips # seems broken??
" #Bundle: kevinw/pyflakes-vim
"
" # <leader>l[frbgj] -- \lj seems a bit breakable...
" Bundle: sjbach/lusty

set nocompatible                          " not compatible with VI
set ff=unix                               " unix file formats
set bs=2                                  " comfortable backspacing
set hidden

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
" default to 8 spaces for a tab
call Tabs(8)

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
set guifont=Monospace\ 11
"set guifont=Envy\ Code\ R\ 11
" and for windows...
"set guifont=Courier_New:h10:cANSI
"set guifont=Envy_Code_R:h10

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
nnoremap <Leader>t :TlistToggle<CR>
nnoremap <Leader>f :NERDTreeToggle<CR>
nnoremap <Leader>g :NERDTreeToggle ~/Dropbox/GTD<CR>
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
if has('matchadd')
    autocmd BufEnter *.c,*.php,*.py,*.java call matchadd('TODO', '\(\t\|[\t ]\+$\)')
endif
autocmd BufWrite *.c,*.php,*.py,*.java call CleanWhitespace()

" detect vim >= 7.3
if !has('conceal')
    finish
endif
autocmd BufEnter * set colorcolumn=80

function! LowerCase(word)
        exec "%s/\\<" . a:word . "\\>/\\l&/ge"
endfunction

function! RipCase()
        call LowerCase("A")
        call LowerCase("As")
        call LowerCase("At")
        call LowerCase("Be")
        call LowerCase("By")
        call LowerCase("In")
        call LowerCase("Is")
        call LowerCase("It")
        call LowerCase("Of")
        call LowerCase("On")
        call LowerCase("Or")
        call LowerCase("So")
        call LowerCase("To")
        call LowerCase("And")
        call LowerCase("But")
        call LowerCase("For")
        call LowerCase("The")
        call LowerCase("Are")
        call LowerCase("From")
        call LowerCase("Into")
        call LowerCase("That")
        call LowerCase("Them")
        call LowerCase("They")
        call LowerCase("This")
        call LowerCase("With")
        %s/=[a-z]/\U&/e
        %s#/ [a-z]#\U&#e
endfunction

call pathogen#runtime_append_all_bundles()
