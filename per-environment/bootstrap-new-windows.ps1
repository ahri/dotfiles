iex (new-object net.webclient).downloadstring('https://get.scoop.sh')
scoop bucket add extras
scoop install 7zip vim openssh git grep touch curl wget gawk concfg pshazz atom stack python ruby nodejs nmap msys2
[environment]::setenvironmentvariable('GIT_SSH', (resolve-path (scoop which ssh)), 'USER')
gem install rake
npm install -g elm
stack install stylish-haskell ghc-mod
apm install language-haskell haskell-ghc-mod ide-haskell-cabal ide-haskell autocomplete-haskell
concfg import monokai small
