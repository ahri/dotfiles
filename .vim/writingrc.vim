source ~/.vim/common.vim

if filereadable($HOME . "/.vim/autoload/plug.vim")
        call plug#begin(BundleDir())
        source ~/.vim/common-deps.vim
        source ~/.vim/writing-deps.vim
        call plug#end()
endif

if has("win32")
        set anti guifont=Fantasque\ Sans\ Mono\ Regular:h18
elseif has("gui_macvim")
        set anti guifont=Fantasque\ Sans\ Mono\ Regular:h32
else
        set anti guifont=Fantasque\ Sans\ Mono\ 15
endif

set background=light
set wrap
set nonumber
set norelativenumber
set guioptions=aei
set mouse=

set spell spelllang=en_gb
nnoremap ]z ]s
nnoremap [z [s
" ]z and [z to navigate through spelling errors
" z= to suggest a change
" zg to mark a word "good"
" zw to mark a work "wrong"
" zug, zuw - undo

colorscheme pencil

filetype plugin indent on

augroup writing
	autocmd!
	autocmd VimEnter * call lexical#init()
	autocmd VimEnter * call textobj#sentence#init()
	autocmd VimEnter * call textobj#quote#init()
	autocmd BufEnter *.md setlocal syntax=markdown | setlocal textwidth=80
augroup END

autocmd! User GoyoLeave nested quit

let g:netrw_banner=0

function! Focus()
        Goyo 90x40
        Limelight
endfunction

" TODO: consider https://github.com/vim-scripts/LanguageTool

nnoremap <leader>f :call Focus()<CR>
nnoremap <leader>q gqip "  hard re-wrap paragraph

nnoremap <leader>b :cd ~/repos/blog \| silent edit _posts/ \| normal r<cr>
