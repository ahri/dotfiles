call Tabs(4)

set wildignore+=*.hi,*.o,*.js_dyn_hi,*.js_dyn_o,*.js_hi,*.js_o,*.dyn_hi,*.dyn_o,*.cache

let g:ale_linters = { 'haskell': ['hie'] }
let g:ale_fixers = { 'haskell': ['brittany'] }
let g:ale_completion_enabled = 1

" assumes we're using vim-rooter to chdir to the project root
function! HsTags()
	if executable("fast-tags")
		if filereadable("tags")
			!fast-tags %
		else
			!fast-tags -R .
		endif
	else
		echom "fast-tags not found - tags not generated"
	endif
endfunction

augroup tags
	autocmd BufWritePost *.hs,*.lhs silent call HsTags()
augroup END
