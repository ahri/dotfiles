call Tabs(4)

set iskeyword=a-z,A-Z,_,.,39

let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`

" NecoGhc - unfortunately doesn't complete local stuff, only installed modules :(
" using https://github.com/mkasa/neco-ghc-lushtags as a faster alternative
" with same api

" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
let g:necoghc_enable_detailed_browse = 1 " might be slow
let g:ycm_semantic_triggers = {'haskell' : ['.']}
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

let g:haskell_tabular = 1

nnoremap <leader>td :InteroGoToDef<CR>
nnoremap <leader>tu :InteroUses<CR>
nnoremap <leader>tt :InteroGenericType<CR>
nnoremap <leader>ti :InteroTypeInsert<CR>

" Reload the file in Intero after saving
" autocmd! BufWritePost *.hs InteroReload

if executable('fast-tags')
	au BufWritePost *.hs            silent !init-tags %
	au BufWritePost *.hsc           silent !init-tags %
	nnoremap <silent> <c-]> :setl iskeyword=@,_,.,48-57,39<cr><c-]>
	    \:setl iskeyword=@,48-57,_,192-255<cr>
endif

if executable('ghcid')
	let g:ghcid_command = 'ghcid -c "stack ghci" -C server'
endif
