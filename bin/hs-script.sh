#!/bin/sh

set -ue

if [ $# -ne 1 ]; then
	echo "Specify a script name" 1>&2
	exit 1
fi

script="$1"

cat <<EOF > "$script"
#!/usr/bin/env stack
{- stack --resolver lts-11.7 --install-ghc runghc
   --package containers
   --package regex-compat
   --package process
   --package directory
-}

-- https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/using-warnings.html
{-# OPTIONS_GHC -Werror -Wall -Wcompat                                #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Widentities -Wredundant-constraints                  #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction -Wmissing-home-modules     #-}
{-# OPTIONS_GHC -fno-warn-unused-imports                              #-}
-- {-# OPTIONS_GHC -ddump-minimal-imports                             #-}

import             Data.Semigroup
import             Data.Foldable
import             Data.Traversable
import             System.Directory
import             System.Environment
import             System.Exit
import             System.IO
import             System.Process
import             Text.Regex

main :: IO ()
main = do
    putStrLn "stdin:"
    getContents >>= putStrLn

    args <- getArgs
    putStrLn "args:"
    traverse_ (hPutStrLn stderr) args 

    exitFailure
EOF

chmod 755 "$script"
