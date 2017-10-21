Plug 'airblade/vim-rooter'

if has('nvim')
	Plug 'neomake/neomake'
else
	Plug 'scrooloose/syntastic'
	set statusline+=%#warningmsg#
	set statusline+=%{SyntasticStatuslineFlag()}
	set statusline+=%*

	let g:syntastic_always_populate_loc_list = 1
	let g:syntastic_auto_loc_list = 0
	let g:syntastic_check_on_open = 0
	let g:syntastic_check_on_wq = 0

	if executable('make')
		Plug 'Shougo/vimproc.vim', { 'do': 'make' }
	endif
endif

Plug 'chrisbra/NrrwRgn' " select a region and do :NR, then save to return

if has('nvim') && has('python3') && executable('pip3')
	Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
	let g:deoplete#enable_at_startup = 1
elseif has('lua')
	Plug 'Shougo/neocomplete.vim'
	let g:neocomplete#enable_at_startup = 1
elseif has('python') && executable('python')
	Plug 'Valloric/YouCompleteMe', { 'do': 'python install.py' }
else
	Plug 'ajh17/VimCompletesMe'
endif

Plug 'terryma/vim-expand-region'
map K <Plug>(expand_region_expand)
map J <Plug>(expand_region_shrink)

Plug 'terryma/vim-multiple-cursors'
" ctrl+n
" ctrl+p - go back
" ctrl+x - exclude this one

Plug 'kien/ctrlp.vim'
let g:ctrlp_custom_ignore = {
	\ 'dir':  '\v[\/](\.([^/]+))$',
	\ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg|.pyc)$',
	\}
let g:ctrlp_show_hidden = 1
let g:ctrlp_working_path_mode = 'ra' " use .git as the root
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/.tmp/*,*/.sass-cache/*,*/node_modules/*,*.keep,*.DS_Store,*/.git/*

nnoremap <leader>o :CtrlP<cr>
nnoremap <leader>e :CtrlPBuffer<cr>

let g:ctrlp_buffer_func = { 'enter': 'CtrlPMappings' }
function! CtrlPMappings()
        nnoremap <buffer> <silent> <C-@> :call <sid>DeleteBuffer()<cr>
endfunction
function! s:DeleteBuffer()
        let path = fnamemodify(getline('.')[2:], ':p')
        let bufn = matchstr(path, '\v\d+\ze\*No Name')
        exec 'bd' bufn ==# '' ? path : bufn
        exec 'norm \<F5>'
endfunction

Plug 'airblade/vim-gitgutter'
nnoremap <C-J> :GitGutterNextHunk<cr>
nnoremap <C-K> :GitGutterPrevHunk<cr>
nnoremap <C-L> :GitGutterStageHunk<cr>
nnoremap <C-H> :GitGutterUndoHunk<cr>
if has('nvim')
	nmap <BS> <C-W>h
endif

" use \\\ to comment stuff
Plug 'tpope/vim-commentary'
nmap \\\ <Plug>CommentaryLine
vmap \\ <Plug>Commentary

if executable('ctags')
	Plug 'ludovicchabant/vim-gutentags'
	Plug 'majutsushi/tagbar' " :TagbarToggle
	let g:tagbar_autofocus = 1
endif

" Haskell stuff
" Plug 'neovimhaskell/haskell-vim'
if executable('stack')
	Plug 'Twinside/vim-hoogle', { 'do' : 'stack install hoogle && hoogle generate' }
	if has('nvim')
		Plug 'parsonsmatt/intero-neovim'
		let g:intero_stack_yaml = 'intero_stack.yaml'
	else
		Plug 'eagletmt/ghcmod-vim', { 'do' : 'stack install hlint ghc-mod' }
		Plug 'eagletmt/neco-ghc'
		if executable('ghcid')
			Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
		endif
	endif
endif

" Elm stuff
if executable('elm')
	Plug 'elmcast/elm-vim'
endif

"Java stuff
if executable('javac')
	Plug 'artur-shaik/vim-javacomplete2'
endif


" IDE-like stuff: defaults
" Type stuff
nnoremap <leader>td :echoerr "UNDEFINED: go to definition"<CR>
nnoremap <leader>tu :echoerr "UNDEFINED: show uses"<CR>
nnoremap <leader>tt :echoerr "UNDEFINED: infer type"<CR>
nnoremap <leader>ti :echoerr "UNDEFINED: type insert"<CR>
" Refactorings
nnoremap <leader>ri :echoerr "UNDEFINED: refactor: inline"<CR>
nnoremap <leader>rr :echoerr "UNDEFINED: refactor: rename"<CR>
nnoremap <leader>rm :echoerr "UNDEFINED: refactor: move"<CR>
" Introduce
nnoremap <leader>iv :echoerr "UNDEFINED: introduce: variable"<CR>
nnoremap <leader>ip :echoerr "UNDEFINED: introduce: parameter"<CR>
nnoremap <leader>if :echoerr "UNDEFINED: introduce: field"<CR>
nnoremap <leader>ic :echoerr "UNDEFINED: introduce: constant"<CR>
" Extract
nnoremap <leader>xm :echoerr "UNDEFINED: extract: method"<CR>
nnoremap <leader>xi :echoerr "UNDEFINED: extract: interface"<CR>
nnoremap <leader>xf :echoerr "UNDEFINED: extract: function"<CR>
