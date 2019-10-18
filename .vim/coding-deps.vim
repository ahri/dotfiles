Plug 'airblade/vim-rooter' " working dir set to project root

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

function! SupportsAsync()
	return (has('nvim') || v:version >= 800)
endfunction

if (SupportsAsync())
	Plug 'w0rp/ale'
	let g:ale_fix_on_save=0
	let g:ale_enabled=0
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

" (Optional) Multi-entry selection UI.
Plug 'junegunn/fzf'

" Required for operations modifying multiple buffers like rename.
set hidden

" TODO: for each lang look for some file that implies support might be useful, add to map
let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie', '--lsp'],
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'javascript.jsx': ['javascript-typescript-stdio'],
    \ }

nnoremap <leader>td :call LanguageClient_textDocument_definition()<CR>
nnoremap <leader>ti :call LanguageClient_textDocument_hover()<CR>
nnoremap <leader>rr :call LanguageClient_textDocument_rename()<CR>
" TODO: the commands did use <silent>
" TODO: check out other LanguageClient_* commands

Plug 'chris-bacon/haskell-refactor'
