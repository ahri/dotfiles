source ~/.vim/common.vim

syntax on

set background=dark
let macvim_skip_colorscheme=1
silent! set macligatures

function! DefaultColorscheme(scheme)
	try
		if (g:colors_name == "default")
			execute 'silent! colorscheme ' . a:scheme
		endif
	catch /\<E121\>/
		execute 'silent! colorscheme ' . a:scheme
	endtry
endfunction

if filereadable($HOME . "/.vim/autoload/plug.vim")
	call plug#begin(BundleDir())
	source ~/.vim/common-deps.vim
	source ~/.vim/coding-deps.vim
	call plug#end()
endif

if has('gui_running') || $TERM =~ "-256color$"
	call DefaultColorscheme("tender")
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
		set anti guifont=Fira_Code:h11
		set encoding=utf8
	elseif has("gui_macvim")
		set anti guifont=FiraCode-Regular:h16
	else
		set anti guifont=FiraCode\ Regular\ 13
	endif
endif

augroup filetypes
	autocmd!
	autocmd BufEnter * :syntax sync fromstart " don't be clever about syntax, just parse the whole file
augroup END

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

" Allow :lnext to work with empty location list, or at last location
function! <SID>LocationNext()
  try
    lnext
  catch /:E553:/
    lfirst
  catch /:E42:/
    echo "Location list empty"
  catch /.*/
    echo v:exception
  endtry
endfunction

nnoremap <silent> <Plug>LocationNext :<C-u>exe 'call <SID>LocationNext()'<CR>
nmap <silent> <C-e> <Plug>LocationNext

set completeopt=menuone,menu,longest " noinsert?

if has('matchadd')
        augroup todo
                autocmd!
                autocmd BufEnter * call matchadd('TODO', '\(\t\|[\t ]\+$\)')
	augroup END
endif
