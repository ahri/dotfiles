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

	" Plug 'zxqfl/tabnine-vim'

	Plug 'neoclide/coc.nvim', {'branch': 'release'}
	" if hidden is not set, TextEdit might fail.
	set hidden

	" Some servers have issues with backup files, see #649
	set nobackup
	set nowritebackup

	" Better display for messages
	set cmdheight=2

	" You will have bad experience for diagnostic messages when it's default 4000.
	set updatetime=300

	" don't give |ins-completion-menu| messages.
	set shortmess+=c

	" always show signcolumns
	set signcolumn=yes

	" Use tab for trigger completion with characters ahead and navigate.
	" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
	inoremap <silent><expr> <TAB>
	      \ pumvisible() ? "\<C-n>" :
	      \ <SID>check_back_space() ? "\<TAB>" :
	      \ coc#refresh()
	inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

	function! s:check_back_space() abort
	  let col = col('.') - 1
	  return !col || getline('.')[col - 1]  =~# '\s'
	endfunction

	" Use <c-space> to trigger completion.
	inoremap <silent><expr> <c-space> coc#refresh()

	" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
	" Coc only does snippet and additional edit on confirm.
	inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

	" Use `[g` and `]g` to navigate diagnostics
	nmap <silent> [g <Plug>(coc-diagnostic-prev)
	nmap <silent> ]g <Plug>(coc-diagnostic-next)

	" Remap keys for gotos
	nmap <silent> gd <Plug>(coc-definition)
	nmap <silent> gy <Plug>(coc-type-definition)
	nmap <silent> gi <Plug>(coc-implementation)
	nmap <silent> gr <Plug>(coc-references)

	" Use K to show documentation in preview window
	nnoremap <silent> K :call <SID>show_documentation()<CR>

	function! s:show_documentation()
	  if (index(['vim','help'], &filetype) >= 0)
	    execute 'h '.expand('<cword>')
	  else
	    call CocAction('doHover')
	  endif
	endfunction

	" Highlight symbol under cursor on CursorHold
	autocmd CursorHold * silent call CocActionAsync('highlight')

	" Remap for rename current word
	nmap <leader>rn <Plug>(coc-rename)

	" Remap for format selected region
	xmap <leader>f  <Plug>(coc-format-selected)
	nmap <leader>f  <Plug>(coc-format-selected)

	augroup mygroup
	  autocmd!
	  " Setup formatexpr specified filetype(s).
	  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
	  " Update signature help on jump placeholder
	  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
	augroup end

	" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
	xmap <leader>a  <Plug>(coc-codeaction-selected)
	nmap <leader>a  <Plug>(coc-codeaction-selected)

	" Remap for do codeAction of current line
	nmap <leader>ac  <Plug>(coc-codeaction)
	" Fix autofix problem of current line
	nmap <leader>qf  <Plug>(coc-fix-current)

	" Create mappings for function text object, requires document symbols feature of languageserver.

	xmap if <Plug>(coc-funcobj-i)
	xmap af <Plug>(coc-funcobj-a)
	omap if <Plug>(coc-funcobj-i)
	omap af <Plug>(coc-funcobj-a)

	" Use <tab> for select selections ranges, needs server support, like: coc-tsserver, coc-python
	nmap <silent> <TAB> <Plug>(coc-range-select)
	xmap <silent> <TAB> <Plug>(coc-range-select)
	xmap <silent> <S-TAB> <Plug>(coc-range-select-backword)

	" Use `:Format` to format current buffer
	command! -nargs=0 Format :call CocAction('format')

	" Use `:Fold` to fold current buffer
	command! -nargs=? Fold :call     CocAction('fold', <f-args>)

	" use `:OR` for organize import of current buffer
	command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

	" Add status line support, for integration with other plugin, checkout `:h coc-status`
	set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

	" Using CocList
	" Show all diagnostics
	nnoremap <silent> <leader>ca  :<C-u>CocList diagnostics<cr>
	" Manage extensions
	nnoremap <silent> <leader>ce  :<C-u>CocList extensions<cr>
	" Show commands
	nnoremap <silent> <leader>cc  :<C-u>CocList commands<cr>
	" Find symbol of current document
	nnoremap <silent> <leader>co  :<C-u>CocList outline<cr>
	" Search workspace symbols
	nnoremap <silent> <leader>cs  :<C-u>CocList -I symbols<cr>
	" Do default action for next item.
	nnoremap <silent> <leader>cj  :<C-u>CocNext<CR>
	" Do default action for previous item.
	nnoremap <silent> <leader>ck  :<C-u>CocPrev<CR>
	" Resume latest coc list
	nnoremap <silent> <leader>cp  :<C-u>CocListResume<CR>
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
