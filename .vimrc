autocmd ColorScheme * highlight Todo ctermbg=darkyellow guibg=darkyellow

source ~/.vim/common.vim

syntax on

set background=dark
let macvim_skip_colorscheme=1

if has("termguicolors")
	set termguicolors
endif

" For Neovim 0.1.3 and 0.1.4
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

if !has('gui_running') && $TERM =~ "-256color"
	set t_Co=256 " breaks my mintty settings
endif

function! DefaultColorscheme(scheme)
	try
		if (g:colors_name == "default")
			execute 'colorscheme ' . a:scheme
		endif
	catch /\<E121\>/
		execute 'colorscheme ' . a:scheme
	endtry
endfunction

if filereadable($HOME . "/.vim/autoload/plug.vim")
	call plug#begin(BundleDir())
	source ~/.vim/common-deps.vim
	source ~/.vim/coding-deps.vim
	call plug#end()
endif

if has('gui_running') || $TERM =~ "-256color$"
	call DefaultColorscheme("happy_hacking")
else
	call DefaultColorscheme("elflord")
endif

function! Tabs(spaces)
	" number of spaces tab counts as
	exec "setlocal softtabstop=" . a:spaces
	" auto-indent shift
	exec "setlocal shiftwidth=" . a:spaces
	" how many spaces do tabs look like?
	exec "setlocal tabstop=" . a:spaces
	" expand a tab into spaces
	setlocal expandtab
endfunction

function! CleanWhitespace()
	retab
	%s/ \+$//e
endfunction

set ai				" auto indent
set tw=0			" text-width, set to zero - I don't want line breaks leaking in
set nowrap			" i just don't like it wrapping
set number			" number lines
set relativenumber
set nocuc nocul			" vt/hz line highlight -- I'm always losing my place
"syn sync minlines=500		" look back 500 lines to figure out syntax (may be better than above if slowdown occurs)

" 'look' oriented settings
set ruler			" it's nice to know where you are in life

if has("gui_running")
	if has("win32")
		set anti guifont=Powerline_Consolas:h12:cANSI
		set encoding=utf8
	elseif has("gui_macvim")
		set anti guifont=Monaco_for_Powerline:h18
	else
		set anti guifont=Fantasque\ Sans\ Mono\ 13
	endif
endif

augroup filetypes
	autocmd!
	autocmd BufEnter * :syntax sync fromstart " don't be clever about syntax, just parse the whole file
augroup END

if has('matchadd')
	augroup todo
		autocmd!
		autocmd BufEnter * silent! call matchadd('Todo', 'TODO:\|AMP:', -1)
	augroup END
endif

if has('conceal')
	augroup margin
		autocmd!
		autocmd BufEnter * set colorcolumn=80
	augroup END
endif

" Mapping in vim:
" map, noremap (non recursive map, similar to by-reference passing) - normal, visual & operator modes
" nmap = only normal mode
" vmap = only visual mode

nnoremap <leader>vr :so $MYVIMRC<cr>
nnoremap <leader>ve :e $MYVIMRC<cr>

nnoremap <leader>k <C-w>k
nnoremap <leader>j <C-w>j
nnoremap <leader>h <C-w>h
nnoremap <leader>l <C-w>l
