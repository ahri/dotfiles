" TODO: test out neomake instead of syntastic
Plug 'scrooloose/syntastic'
let g:syntastic_always_populate_loc_list=1

Plug 'ajh17/VimCompletesMe'

if executable('elm')
	Plug 'elmcast/elm-vim'
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
	\ 'dir':  '\v[\/](\.(git|hg|svn))$',
	\ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg|.pyc)$',
	\}
let g:ctrlp_show_hidden = 1
let g:ctrlp_working_path_mode = 'ra' " use .git as the root
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/.tmp/*,*/.sass-cache/*,*/node_modules/*,*.keep,*.DS_Store,*/.git/*

nnoremap <leader>o :CtrlP<cr>
nnoremap <leader>e :CtrlPBuffer<cr>
nnoremap <leader>r :CtrlPMRUFiles<cr>

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

if executable('ctags')
	Plug 'ludovicchabant/vim-gutentags'
endif
