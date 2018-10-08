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
-- {-# OPTIONS_GHC -ddump-minimal-imports                                  #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Text hiding (length, take, drop, words, unwords)
import Data.Text.IO hiding (hGetLine)
import NeatInterpolation
import Prelude hiding (writeFile)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO hiding (writeFile)
import System.Info
import System.Process
import Text.Printf
import Text.Regex.Posix

resolver :: Text
resolver = "lts-12.9"

template :: Text
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
usage = die $ unpack [text|
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
    writeFile scriptName template

sh :: CreateProcess -> IO ExitCode
sh cp = withCreateProcess cp (\_ _ _ ph -> waitForProcess ph)

repl :: String -> IO ()
repl scriptName = do
    cmd <- getCmdWithReplacement scriptName "ghci"
    exitCode <- sh . shell $ cmd ++ " " ++ scriptName
    exitWith exitCode

watch :: String -> IO ()
watch scriptName = do
    cmd <- getCmdWithReplacement scriptName "ghci"
    exitCode <- sh . shell $ "ghcid -c '" ++ cmd ++ " " ++ scriptName ++ "'"
    exitWith exitCode

lint :: String -> IO ()
lint scriptName = do
    exitCode <- sh . shell $ "hlint " ++ scriptName
    exitWith exitCode

fixLinting :: String -> IO ()
fixLinting scriptName = do
    exitCode <- sh . shell $ "hlint --refactor --refactor-options='-is' " ++ scriptName
    exitWith exitCode

compile :: String -> IO ()
compile scriptName = die "TODO: compile"

profile :: String -> [String] -> IO ()
profile scriptName args = die "TODO: profile"

getCmdWithReplacement :: String -> String -> IO String
getCmdWithReplacement scriptName replacement = do
    res <- withFile scriptName ReadMode $ go Nothing
    when (res == Nothing) $ die "Couldn't find params"
    pure . maybe "" id $ res
  where
    go :: Maybe String -> Handle -> IO (Maybe String)
    go state handle = do
        line <- hGetLine handle
        if state /= Nothing && line =~ ("-}"::String)
            then pure $ (++ (unwords . dropLast $ words line)) <$> state
        else if state /= Nothing
            then go (Just $ (maybe "" id state) ++ line) handle
        else if line =~ ("^\\{- stack"::String)
            then go
                (Just $ (maybe "" id state) ++ (unwords . drop 1 . replaceWords "runghc" replacement $ words line))
                handle
        else
            go state handle

    dropLast strs = take (length strs - 1) strs

    replaceWords :: String -> String -> [String] -> [String]
    replaceWords from to strs = [if word == "runghc" then replacement else word | word <- strs]
