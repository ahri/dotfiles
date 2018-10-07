#!/usr/bin/env stack
{- stack --resolver lts-12.9 --install-ghc runghc
    --package directory
    --package containers
    --package filepath
    --package regex-posix
    --package neat-interpolation
    --package text
    --package bytestring
    --package process
-}

{- COMPILE_FLAGS -O2 -threaded -rtsopts -eventlog -static -optl-pthread -optl-static -}

-- https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/using-warnings.html
{-# OPTIONS_GHC -Werror -Wall -Wcompat                                  #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-record-updates   #-}
{-# OPTIONS_GHC -Widentities -Wredundant-constraints                    #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction -Wmissing-home-modules       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches       #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns                       #-}
-- {-# OPTIONS_GHC -ddump-minimal-imports                                                             #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import NeatInterpolation
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Info
import System.Process
import Text.Printf
import Text.Regex.Posix

resolver :: T.Text
resolver = "lts-12.9"

template :: T.Text
template = [text|
	#!/usr/bin/env stack
	{- stack --resolver $resolver --install-ghc runghc
	    --package containers
	    --package regex-posix
	    --package process
	    --package directory
	-}

	{- COMPILE_FLAGS -O2 -threaded -rtsopts -eventlog -}

	-- https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/using-warnings.html
	{-# OPTIONS_GHC -Werror -Wall -Wcompat                                  #-}
	{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-record-updates   #-}
	{-# OPTIONS_GHC -Widentities -Wredundant-constraints                    #-}
	{-# OPTIONS_GHC -Wmonomorphism-restriction -Wmissing-home-modules       #-}

	{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches       #-}
	{-# OPTIONS_GHC -fno-warn-unused-top-binds -fno-warn-unused-local-binds #-}
	-- {-# OPTIONS_GHC -ddump-minimal-imports                               #-}

	import             Data.Semigroup
	import             Data.Foldable
	import             Data.Traversable
	import             System.Directory
	import             System.Environment
	import             System.Exit
	import             System.IO
	import             System.Process
	import             Text.Printf
	import             Text.Regex

	main :: IO ()
	main = do
		putStrLn "stdin:"
		getContents >>= putStrLn

		args <- getArgs
		putStrLn "args:"
		traverse_ (hPutStrLn stderr) args 

		exitFailure
    |]

main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) usage

    let [scriptPath, command] = take 2 args

    setCurrentDirectory $ takeDirectory scriptPath
    getCurrentDirectory >>= print

    let scriptName = takeFileName scriptPath

    let cmdArgs        = drop 2 args
    case command of
        "new"         -> new scriptName
        "repl"        -> repl scriptName
        "watch"       -> watch scriptName
        "lint"        -> lint scriptName
        "fix-linting" -> fixLinting scriptName
        "compile"     -> compile scriptName
        "profile"     -> profile scriptName cmdArgs
        _             -> die $ printf "Unknown command: %s" command

usage :: IO ()
usage = die $ T.unpack [text|
    Usage: script_name command [cmd params]
    Commands:
        new
        repl
        watch
        lint
        fix-linting
        compile
        profile [additional RTS options]
    |]

new :: String -> IO ()
new scriptName = do
    exists <- doesFileExist scriptName
    when exists . die $ printf "Script %s already exists" scriptName
    T.writeFile scriptName template

repl :: String -> IO ()
repl scriptName = die "TODO: repl"

watch :: String -> IO ()
watch scriptName = die "TODO: watch"

lint :: String -> IO ()
lint scriptName = die "TODO: lint"

fixLinting :: String -> IO ()
fixLinting scriptName = die "TODO: fixLinting"

compile :: String -> IO ()
compile scriptName = die "TODO: compile"

profile :: String -> [String] -> IO ()
profile scriptName args = die "TODO: profile"

ghciCmd :: String -> IO String
ghciCmd scriptName = do
    file <- readFile scriptName
    getIt $ unlines file
    -- TODO: use hGetLine on a file handle to read lines
