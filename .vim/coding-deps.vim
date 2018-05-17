Plug 'airblade/vim-rooter' " working dir set to project root

Plug 'chrisbra/NrrwRgn' " select a region and do :NR, then save to return

Plug 'terryma/vim-expand-region'
map K <Plug>(expand_region_expand)
map J <Plug>(expand_region_shrink)

Plug 'terryma/vim-multiple-cursors'
" ctrl+n
" ctrl+p - go back
" ctrl+x - exclude this one

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

" IDE-like stuff: defaults
" Type stuff
nnoremap <leader>td :echoerr "UNDEFINED: go to definition"<CR>
nnoremap <leader>tu :echoerr "UNDEFINED: show uses"<CR>
nnoremap <leader>tt :echoerr "UNDEFINED: infer type"<CR>
nnoremap <leader>ti :echoerr "UNDEFINED: type information"<CR>
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
