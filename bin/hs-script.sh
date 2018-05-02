#!/bin/sh

set -ue

if [ $# -ne 1 ]; then
	echo "Specify a script name" 1>&2
	exit 1
fi

script="$1"

cat <<EOF > "$script"
#!/usr/bin/env stack
-- stack --resolver lts-11.7 --install-ghc runghc --package containers

import             Data.Foldable
import             System.Environment
import             System.Exit
import             System.IO

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
