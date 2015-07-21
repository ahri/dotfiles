source ~/.vim/common.vim

" curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
if filereadable($HOME . "/.vim/autoload/plug.vim")
        call plug#begin(BundleDir())
                for f in split(glob('~/.vim/*-deps.vim'), '\n')
                        execute 'source' f
                endfor
        call plug#end()
endif
