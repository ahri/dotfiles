set nocompatible
set runtimepath=~/.vim,$VIMRUNTIME

set relativenumber " display numbers relative to current line

" Mapping in vim:
" map, noremap (non recursive map, similar to by-reference passing) - normal, visual & operator modes
" nmap = only normal mode
" vmap = only visual mode

let mapleader = "\<Space>"

nnoremap <leader>vr :so $MYVIMRC<cr>
nnoremap <leader>ve :e $MYVIMRC<cr>

" map <leader>d to duplicate text, commenting out the first one
vnoremap <leader>d YP`[V`]:Commentary<cr>`]<cr>
nnoremap <leader>d YP`[V`]:Commentary<cr>`]<cr>

" Disable cursor keys so I'm not tempted
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
inoremap <Right> <NOP>

nnoremap <leader>k <C-w>k
nnoremap <leader>j <C-w>j
nnoremap <leader>h <C-w>h
nnoremap <leader>l <C-w>l

nnoremap j gj
nnoremap k gk

set background=dark

" TODO: consider https://github.com/vim-scripts/LanguageTool
set nospell spelllang=en_gb
nnoremap ]z ]s
nnoremap [z [s
" ]z and [z to navigate through spelling errors
" z= to suggest a change
" zg to mark a word "good"
" zw to mark a work "wrong"
" zug, zuw - undo

nnoremap <leader>q gqip "  hard re-wrap paragraph
nnoremap <leader>p `[v`] " select last paste

autocmd FocusLost * :silent! wall " Write files when focus lost

function! BundleDir(...)
        let bundledir = $HOME . "/.vim/bundle"

        if a:0 == 0
                return bundledir
        endif

        return bundledir . "/" . a:1
endfunction

" curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
if filereadable($HOME . "/.vim/autoload/plug.vim")
        call plug#begin(BundleDir())

        source ~/.vim/common-deps.vim

        function! InstallingOrCompiled(compiled_file)
                let plugin_dir = matchstr(a:compiled_file, '\zs.*\.vim/bundle/[^/]\+\ze/')
                if empty(plugin_dir)
                        throw "no plugin_dir"
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

                let potential_rgbtxt = "/Applications/MacVim.app/Contents/Resources/vim/runtime/rgb.txt"
                if filereadable(potential_rgbtxt)
                        let g:csexact_rgbtxt = potential_rgbtxt
                endif
                Plug 'KevinGoodsell/vim-csexact' " now get as close as possible to gvim's colours (takes longer to start and quit)
                " :CSExactColors (to reset... doesn't seem to work that well in practise!)
        endif

        Plug 'tomasr/molokai'
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

        Plug 'sjl/gundo.vim'
        nnoremap <leader>u :GundoToggle<CR>

        Plug 'vim-scripts/YankRing.vim'
        nnoremap <leader>y :YRShow<cr>

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
                exec "bd" bufn ==# "" ? path : bufn
                exec "norm \<F5>"
        endfunction

        if executable("ag")
                set grepprg=ag\ --nogroup\ --nocolor
                let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden
                      \ --ignore .git
                      \ --ignore .svn
                      \ --ignore .hg
                      \ --ignore .DS_Store
                      \ --ignore "**/*.pyc"
                      \ --ignore "**/*.class"
                      \ -g ""'
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

        " ### Code highlighting
        Plug 'sheerun/vim-polyglot'

        " ### General Code
        " completion
        Plug 'ajh17/VimCompletesMe'

        if InstallingOrCompiled(BundleDir("YouCompleteMe/third_party/ycmd/ycm_core.so")) && (v:version > 703 || (v:version == 703 && has('patch584')))
                Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }
        endif

        if InstallingOrCompiled(BundleDir("tern_for_vim/node_modules/.bin/tern"))
                Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
                nnoremap <leader>tR :TernRename<cr>
                nnoremap <leader>tt :TernType<cr>
                nnoremap <leader>tr :TernRefs<cr>
                nnoremap <leader>td :TernDef<cr>
        endif

        " run linters etc.
        Plug 'scrooloose/syntastic'
        " use \\\ to comment stuff
        Plug 'tpope/vim-commentary'
        Plug 'Raimondi/delimitMate' " add delimiters

        " So that I can manage my deps all in one place
        source ~/.vim/writing-deps.vim

        call plug#end()

        colorscheme molokai
endif

syntax on
set bs=2                                  " comfortable backspacing
set backspace=indent,eol,start
set hidden                                " allow modified buffers to be hidden
set smartcase                             " ignore case in searches unless there's a capital in the search
set ignorecase

" anti-tab settings
function! Tabs(spaces)
        " number of spaces tab counts as
        exec "setlocal softtabstop=" . a:spaces
        " auto-indent shift
        exec "setlocal shiftwidth=" . a:spaces
        " how many spaces do tabs look like?
        exec "setlocal tabstop=" . a:spaces
        " expand a tab into spaces
        setlocal expandtab
endfunction

" default to 4 spaces for a tab
call Tabs(4)

" clean up the whitespace in a file, as long as it's not switched off with:
" :let g:unclean=1
function! CleanWhitespace()
        if g:dirty == 0
                retab
                %s/ \+$//e
        endif
endfunction
let g:dirty = 0

function! Dirty()
        let g:dirty = 1
endfunction

" backup/recovery related
set nobackup
set noswapfile

" coding oriented settings
set ai                                    " auto indent
set tw=0                                  " text-width, set to zero - I don't want line breaks leaking in
set nowrap                                " i just don't like it wrapping
set nu                                    " number lines
set nocuc nocul                           " vt/hz line highlight -- I'm always losing my place
autocmd BufEnter * :syntax sync fromstart " don't be clever about syntax, just parse the whole file
"syn sync minlines=500                     " look back 500 lines to figure out syntax (may be better than above if slowdown occurs)

" 'look' oriented settings
set ruler                                 " it's nice to know where you are in life
set showcmd                               " show command in status line
set incsearch                             " incremental searching - ie. search-as-you-type
set scrolloff=6                           " lines above/below to show for context

if has("gui_running")
	set gcr=a:blinkon0                        " stop the cursor blinking in GUI mode
	set guioptions=aei                        " set a few gui options
	set mouse=                                " disable the mouse when --with-x was specified

	if has("win32")
		set anti guifont=Monaco_for_Powerline:h12:cANSI
                set encoding=utf8
	elseif has("gui_macvim")
		set anti guifont=Monaco_for_Powerline:h18
	else
		set anti guifont=Monaco\ for\ Powerline\ 11
	endif
endif

" Don't use ex mode
noremap Q <NOP>

" set some stuff up per filetype
autocmd BufEnter *.py call Tabs(4)
autocmd BufEnter *.rb call Tabs(2)
autocmd BufEnter *.js call Tabs(2)
autocmd BufEnter *.md setlocal textwidth=80
autocmd BufEnter *.vim* call Tabs(8)
autocmd BufEnter Rakefile set syntax=ruby | call Tabs(2)
autocmd BufEnter Buildfile set syntax=ruby | call Tabs(2)
autocmd BufEnter build.gradle set syntax=groovy
if has('matchadd')
    autocmd BufEnter * call matchadd('TODO', '\(\t\|[\t ]\+$\)')
endif
autocmd BufWrite *.c,*.php,*.py,*.rb,*.java,*.js call CleanWhitespace()

" detect vim >= 7.3
if !has('conceal')
    finish
endif
autocmd BufEnter * set colorcolumn=80
