Plug 'moll/vim-bbye' " Use :Bd to remove buffers neatly
Plug 'godlygeek/tabular' " align on commas: :Tabular /,
Plug 'tpope/vim-abolish'
Plug 'farmergreg/vim-lastplace'
" :Abolish {despa,sepe}rat{e,es,ed,ing,ely,ion,ions,or}  {despe,sepa}rat{}
" :%Subvert/facilit{y,ies}/building{,s}/g
" :Subvert/address{,es}/reference{,s}/g
" :Subvert/blog{,s}/post{,s}/g
" :Subvert/child{,ren}/adult{,s}/g
" :Subvert/di{e,ce}/spinner{,s}/gc ---- c for "confirm"
" move cursor over word and convert with coercions:
"  MixedCase (crm), camelCase (crc), snake_case (crs), UPPER_CASE (cru),
"  dash-case (cr-), dot.case (cr.), space case (cr<space>), and Title Case
"  (crt) 

Plug 'kien/ctrlp.vim'
let g:ctrlp_custom_ignore = {
	\ 'dir':  '\v[\/](\.([^/]+))$',
	\ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg|.pyc)$',
	\}
let g:ctrlp_show_hidden = 1
let g:ctrlp_working_path_mode = 'ra' " use .git as the root

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
