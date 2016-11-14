" TODO: test out neomake instead of syntastic
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
nnoremap <C-H> :GitGutterRevertHunk<cr>

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
	Plug 'eagletmt/ghcmod-vim', { 'do' : 'stack install hlint ghc-mod' }
	Plug 'eagletmt/neco-ghc'
endif

" Elm stuff
if executable('elm')
	Plug 'elmcast/elm-vim'
endif
