source ~/.vim/common.vim

if filereadable($HOME . "/.vim/autoload/plug.vim")
        call plug#begin(BundleDir())
        source ~/.vim/common-deps.vim
        source ~/.vim/coding-deps.vim
        call plug#end()

        colorscheme molokai
endif

syntax on
set background=dark

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

" coding oriented settings
set ai                                    " auto indent
set tw=0                                  " text-width, set to zero - I don't want line breaks leaking in
set nowrap                                " i just don't like it wrapping
set nu                                    " number lines
set nocuc nocul                           " vt/hz line highlight -- I'm always losing my place
"syn sync minlines=500                     " look back 500 lines to figure out syntax (may be better than above if slowdown occurs)

" 'look' oriented settings
set ruler                                 " it's nice to know where you are in life

if has("gui_running")
	if has("win32")
		set anti guifont=Powerline_Consolas:h12:cANSI
                set encoding=utf8
	elseif has("gui_macvim")
		set anti guifont=Monaco_for_Powerline:h18
	else
		set anti guifont=Monaco\ for\ Powerline\ 11
	endif
endif

" set some stuff up per filetype
augroup filetypes
        autocmd!
        autocmd BufEnter * :syntax sync fromstart " don't be clever about syntax, just parse the whole file
        autocmd BufEnter *.py call Tabs(4)
        autocmd BufEnter *.rb call Tabs(2)
        autocmd BufEnter *.js call Tabs(2)
        autocmd BufEnter *.md setlocal textwidth=80
        autocmd BufEnter *.vim* call Tabs(8)
        autocmd BufEnter Rakefile set syntax=ruby | call Tabs(2)
        autocmd BufEnter Buildfile set syntax=ruby | call Tabs(2)
        autocmd BufEnter build.gradle set syntax=groovy
        autocmd BufWrite *.c,*.php,*.py,*.rb,*.java,*.js call CleanWhitespace()
augroup END

if has('matchadd')
        augroup todo
                autocmd!
                autocmd BufEnter * call matchadd('TODO', '\(\t\|[\t ]\+$\)')
        augroup END
endif

if has('conceal')
        augroup margin
                autocmd!
                autocmd BufEnter * set colorcolumn=80
        augroup END
endif

set relativenumber " display numbers relative to current line

" Mapping in vim:
" map, noremap (non recursive map, similar to by-reference passing) - normal, visual & operator modes
" nmap = only normal mode
" vmap = only visual mode

nnoremap <leader>vr :so $MYVIMRC<cr>
nnoremap <leader>ve :e $MYVIMRC<cr>

nnoremap <leader>k <C-w>k
nnoremap <leader>j <C-w>j
nnoremap <leader>h <C-w>h
nnoremap <leader>l <C-w>l

" noremap <leader>o o<Esc>k
" noremap <leader>O O<Esc>j

nnoremap <leader>p `[v`] " select last paste
