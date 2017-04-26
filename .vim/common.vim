set nocompatible
set runtimepath=~/.vim,$VIMRUNTIME

let mapleader = "\<Space>"

" Don't use ex mode
noremap Q <NOP>

" backup/recovery related
set nobackup
set noswapfile

set showcmd                               " show command in status line
set incsearch                             " incremental searching - ie. search-as-you-type
set scrolloff=6                           " lines above/below to show for context
set hidden                                " allow modified buffers to be hidden
set smartcase                             " ignore case in searches unless there's a capital in the search
set ignorecase
set cursorline                            " highlight current line
set cursorcolumn                          " highlight current column

" Get rid of any bells
set noerrorbells
set vb t_vb=

" Don't redraw while executing macros (good performance config)
set lazyredraw

" Use pleasant but very visible search hilighting
hi Search ctermfg=white ctermbg=173 cterm=none guifg=#ffffff guibg=#e5786d gui=none
hi! link Visual Search

" Searing red very visible cursor
hi Cursor guibg=red

if has("gui_running")
	set gcr=a:blinkon0                        " stop the cursor blinking in GUI mode
	set guioptions=aei                        " set a few gui options
endif

if has("mouse")
	set mouse=                                " disable the mouse when --with-x was specified
endif

function! BundleDir(...)
        let bundledir = $HOME . "/.vim/bundle"

        if a:0 == 0
                return bundledir
        endif

        return bundledir . "/" . a:1
endfunction

" Habit breaking/making
set backspace= " use vi backspace behaviour. gJ in normal mode will join lines

noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
inoremap <Right> <NOP>

" when wrapped, move to the expected location
nnoremap j gj
nnoremap k gk
