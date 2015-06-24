set nocompatible              " be iMproved, required
filetype off                  " required

let mapleader = "\<Space>"

nnoremap <leader>vr :so $MYVIMRC<cr>
nnoremap <leader>ve :e $MYVIMRC<cr>

" Mapping in vim:
" map, noremap (non recursive map, similar to by-reference passing) - normal, visual & operator modes
" nmap = only normal mode
" vmap = only visual mode

set background=dark

" $ mkdir -p ~/.vim/bundle && git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

if isdirectory($HOME . "/.vim/bundle/Vundle.vim")
        " set the runtime path to include Vundle and initialize
        set rtp+=~/.vim/bundle/Vundle.vim
        call vundle#begin()

        " let Vundle manage Vundle
        Plugin 'gmarik/Vundle.vim'

        " See http://vim-scripts.org
        " See https://github.com/vim-scripts/

        function! InstallingOrCompiled(compiled_file)
                let plugin_dir = matchstr(a:compiled_file, '\zs.*\.vim/bundle/[^/]\+\ze/')
                if empty(plugin_dir)
                        throw "no plugin_dir"
                endif
                return !isdirectory(plugin_dir) || filereadable(a:compiled_file)
        endfunction

        " ### Look & Feel
        Plugin 'bling/vim-airline'
        let g:airline_powerline_fonts = 1
        let g:airline#extensions#tabline#enabled = 1
        let g:airline#extensions#tabline#fnamemod = ':t'
        set laststatus=2

        if &t_Co >= 88
                Plugin 'CSApprox' " approximate gvim colours
                " :CSApproxSnapshot ~/.vim/colors/foobar.vim
                " :colorscheme foobar

                Plugin 'KevinGoodsell/vim-csexact' " now get as close as possible to gvim's colours (takes longer to start and quit)
                " :CSExactColors (to reset... doesn't seem to work that well in practise!)
        endif

        Plugin 'tomasr/molokai'
        Plugin 'nanotech/jellybeans.vim'
        Plugin 'altercation/vim-colors-solarized'
        Plugin 'chriskempson/base16-vim'
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
        Plugin 'terryma/vim-expand-region'
        map K <Plug>(expand_region_expand)
        map J <Plug>(expand_region_shrink)

        Plugin 'terryma/vim-multiple-cursors'
        " ctrl+n
        " ctrl+p - go back
        " ctrl+x - exclude this one

        Plugin 'kien/ctrlp.vim'
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

        Plugin 'rking/ag.vim'
        nnoremap <leader>s :Ag \\b<cword>\\b<cr>

        Plugin 'justinmk/vim-sneak'
        " Jump to characters: s<chr><chr>, S<chr>chr> (backwards), ; = next,
        " 3; = next*3, 3dzqt = delete up until the 3rd instance of qt

        " ### Source Control
        Plugin 'airblade/vim-gitgutter'
        nnoremap <C-J> :GitGutterNextHunk<cr>
        nnoremap <C-K> :GitGutterPrevHunk<cr>
        nnoremap <C-L> :GitGutterStageHunk<cr>
        nnoremap <C-H> :GitGutterRevertHunk<cr>
        nnoremap <leader>gh :GitGutterPreviewHunk<cr>

        Plugin 'tpope/vim-fugitive'
        nnoremap <leader>gs :Gstatus<cr>
        nnoremap <leader>gb :Gblame<cr>
        nnoremap <leader>gc :Gcommit<cr>
        nnoremap <leader>gl :Glog<cr>
        nnoremap <leader>gj :Gpull<cr>
        nnoremap <leader>gk :Gpush<cr>
        nnoremap <leader>gd :Gdiff<cr>
        nnoremap <leader>gm :Gmerge<cr>

        " ### Code highlighting
        Plugin 'markdown'
        Plugin 'html5.vim'
        Plugin 'pangloss/vim-javascript'
        Plugin 'jelera/vim-javascript-syntax'

        " ### General Code
        " completion
        if InstallingOrCompiled($HOME . "/.vim/bundle/YouCompleteMe/third_party/ycmd/ycm_core.so") && (v:version > 703 || (v:version == 703 && has('patch584')))
                Plugin 'Valloric/YouCompleteMe' " Auto-completion, intellisense-style, requires compilation
        endif

        Plugin 'ajh17/VimCompletesMe' " Tab completion without compiling stuff

        " tern js
        if InstallingOrCompiled($HOME . "/.vim/bundle/tern_for_vim/node_modules/.bin/tern")
                Plugin 'marijnh/tern_for_vim'
                nnoremap <leader>tR :TernRename<cr>
                nnoremap <leader>tt :TernType<cr>
                nnoremap <leader>tr :TernRefs<cr>
                nnoremap <leader>td :TernDef<cr>
        endif

        " run linters etc.
        Plugin 'scrooloose/syntastic'
        " use \\\ to comment stuff
        Plugin 'commentary.vim'
        Plugin 'Raimondi/delimitMate' " add delimiters

        " ### Writing
        if InstallingOrCompiled($HOME . "/.vim/bundle/vim-livedown/node_modules/.bin/livedown")
                Plugin 'shime/vim-livedown'
                " LivedownPreview
                " LivedownKill
        endif

        call vundle#end()
        filetype plugin indent on " has to be after bundles

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
        exec "set softtabstop=" . a:spaces
        " auto-indent shift
        exec "set shiftwidth=" . a:spaces
        " how many spaces do tabs look like?
        exec "set tabstop=" . a:spaces
        " expand a tab into spaces
        set expandtab
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
	set guioptions=aegirLt                    " set a few gui options
	set mouse=                                " disable the mouse when --with-x was specified

	if has("win32")
		set anti guifont=Monaco_for_Powerline:h12:cANSI
	elseif has("gui_macvim")
		set anti guifont=Monaco_for_Powerline:h18
	else
		set anti guifont=Monaco\ for\ Powerline\ 11
	endif
endif

" Disable cursor keys so I'm not tempted
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
inoremap <Right> <NOP>

" Don't use ex mode
noremap Q <NOP>

" set some stuff up per filetype
autocmd BufEnter *.py call Tabs(4)
autocmd BufEnter *.rb call Tabs(2)
autocmd BufEnter *.js call Tabs(2)
autocmd BufEnter .vimrc call Tabs(8)
autocmd BufEnter Rakefile set syntax=ruby | call Tabs(2)
autocmd BufEnter Buildfile set syntax=ruby | call Tabs(2)
autocmd BufEnter build.gradle set syntax=groovy
if has('matchadd')
    autocmd BufEnter * call matchadd('TODO', '\(\t\|[\t ]\+$\)')
endif
autocmd BufWrite *.c,*.php,*.py,*.rb,*.java call CleanWhitespace()

" detect vim >= 7.3
if !has('conceal')
    finish
endif
autocmd BufEnter * set colorcolumn=80
