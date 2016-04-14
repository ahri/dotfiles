function! InstallingOrCompiled(compiled_file)
        let plugin_dir = matchstr(a:compiled_file, '\zs.*\.vim/bundle/[^/]\+\ze/')
        if empty(plugin_dir)
                throw 'no plugin_dir'
        endif
        return !isdirectory(plugin_dir) || filereadable(a:compiled_file)
endfunction

" ### Look & Feel
Plug 'bling/vim-airline'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
set laststatus=2

if empty(&t_Co) || &t_Co >= 88
        Plug 'CSApprox' " approximate gvim colours
        " :CSApproxSnapshot ~/.vim/colors/foobar.vim
        " :colorscheme foobar

        let potential_rgbtxt = '/Applications/MacVim.app/Contents/Resources/vim/runtime/rgb.txt'
        if filereadable(potential_rgbtxt)
                let g:csexact_rgbtxt = potential_rgbtxt
        endif
        " Plug 'KevinGoodsell/vim-csexact' " now get as close as possible to gvim's colours (takes longer to start and quit)
        " :CSExactColors (to reset... doesn't seem to work that well in practise!)
endif

Plug 'tomasr/molokai'
Plug 'romainl/Apprentice'
Plug 'nanotech/jellybeans.vim'
Plug 'altercation/vim-colors-solarized'
Plug 'chriskempson/base16-vim'
" -bespin
" -eighties
" -embers
" -flat
" -harmonic16
" -mocha
" -monokai
" -ocean
" -railscasts
" -solarized
" -tomorrow
" -twilight

" ### Usability
Plug 'myusuf3/numbers.vim'

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

if executable('ag')
        set grepprg=ag\ --nogroup\ --nocolor
        let g:ctrlp_user_command = "ag %s -i --nocolor --nogroup --hidden
              \ --ignore .git
              \ --ignore .svn
              \ --ignore .hg
              \ --ignore .DS_Store
              \ --ignore '**/*.pyc'
              \ --ignore '**/*.class'
              \ -g ''"
endif

Plug 'rking/ag.vim'
nnoremap <leader>s :Ag \\b<cword>\\b<cr>

Plug 'justinmk/vim-sneak'
" Jump to characters: s<chr><chr>, S<chr>chr> (backwards), ; = next,
" 3; = next*3, 3dzqt = delete up until the 3rd instance of qt

" ### Source Control
Plug 'airblade/vim-gitgutter'
nnoremap <C-J> :GitGutterNextHunk<cr>
nnoremap <C-K> :GitGutterPrevHunk<cr>
nnoremap <C-L> :GitGutterStageHunk<cr>
nnoremap <C-H> :GitGutterRevertHunk<cr>
nnoremap <C-g> :GitGutterPreviewHunk<cr>

Plug 'tpope/vim-fugitive'
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gl :Glog<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gm :Gmerge<cr>

" ### Highlighting
" Plug 'sheerun/vim-polyglot' " breaks HTML5
Plug 'ekalinin/Dockerfile.vim'
Plug 'othree/html5.vim', { 'for': 'html' }
Plug 'othree/yajs.vim', { 'for': 'javascript' }

" ### Completion
if executable('flow')
        Plug 'flowtype/vim-flow', { 'for': 'javascript' }
endif
autocmd BufEnter *.js setl omnifunc=flowcomplete#Complete

" " ### TypeScript
" Plug 'HerringtonDarkholme/yats.vim'
" Plug 'leafgarland/typescript-vim'
" Plug 'Shougo/vimproc.vim', {
" \ 'build' : {
" \     'windows' : 'tools\\update-dll-mingw',
" \     'cygwin' : 'make -f make_cygwin.mak',
" \     'mac' : 'make',
" \     'linux' : 'make',
" \     'unix' : 'gmake',
" \    },
" \ }
" Plug 'Quramy/tsuquyomi'

Plug 'ajh17/VimCompletesMe'
" if InstallingOrCompiled(BundleDir('YouCompleteMe/third_party/ycmd/ycm_core.so')) && executable('cmake') && (v:version > 703 || (v:version == 703 && has('patch584')))
"         Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }
" endif

if InstallingOrCompiled(BundleDir('tern_for_vim/node_modules/.bin/tern')) && executable('npm')
        Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
        nnoremap <leader>tR :TernRename<cr>
        nnoremap <leader>tt :TernType<cr>
        nnoremap <leader>tr :TernRefs<cr>
        nnoremap <leader>td :TernDef<cr>
endif

" run linters etc.
if has('nvim')
        Plug 'benekastah/neomake'
        autocmd! BufWritePost * Neomake
        Plug 'janko-m/vim-test'
else
        Plug 'scrooloose/syntastic'
endif

" use \\\ to comment stuff
Plug 'tpope/vim-commentary'
" map <leader>d to duplicate text, commenting out the first one
vnoremap <leader>d YP`[V`]:Commentary<cr>`]<cr>
nnoremap <leader>d YP`[V`]:Commentary<cr>`]<cr>
