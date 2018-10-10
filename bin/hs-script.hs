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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import qualified Data.Text as T
import NeatInterpolation
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Printf
import Text.Regex.Posix

resolver :: T.Text
resolver = "lts-12.9"

template :: String
template = T.unpack [text|
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

	import Data.Semigroup
	import Data.Foldable
	import Data.Traversable
	import System.Directory
	import System.Environment
	import System.Exit
    import System.FilePath
	import System.IO
	import System.Process
	import Text.Printf
	import Text.Regex

    sh :: CreateProcess -> IO ()
    sh cp = do
        exitCode <- withCreateProcess cp (\_ _ _ ph -> waitForProcess ph)
        when (exitCode /= ExitSuccess) $ exitWith exitCode
        pure ()

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
    noOverwrite scriptName
    writeFile scriptName template
    perms <- getPermissions scriptName
    setPermissions scriptName $ setOwnerExecutable True perms

sh :: CreateProcess -> IO ()
sh cp = do
    exitCode <- withCreateProcess cp (\_ _ _ ph -> waitForProcess ph)
    when (exitCode /= ExitSuccess) $ exitWith exitCode
    pure ()

repl :: String -> IO ()
repl scriptName = do
    cmd <- getRunghcCmdWithReplacement scriptName "ghci"
    sh . shell $ printf "%s %s" cmd scriptName

watch :: String -> IO ()
watch scriptName = do
    cmd <- getRunghcCmdWithReplacement scriptName "ghci"
    sh . shell $ printf "ghcid -c \"%s \\\"%s\\\"\"" cmd scriptName

lint :: String -> IO ()
lint scriptName = do
    sh . shell $ printf "hlint \"%s\"" scriptName

fixLinting :: String -> IO ()
fixLinting scriptName = do
    sh . shell $ printf "hlint --refactor --refactor-options=\"-is\" \"%s\"" scriptName

compile :: String -> IO ()
compile scriptName = do
    cmd <- getRunghcCmdWithReplacement scriptName "ghc"
    flags <- unwords . replaceWords "COMPILE_FLAGS" "" . words . maybe "" id <$> getCompileFlags scriptName
    noOverwrite $ exeName scriptName
    sh . shell $ printf "%s -- %s %s" cmd flags scriptName

profile :: String -> [String] -> IO ()
profile scriptName args = do
    threadscopeExe <- findExecutable "threadscope"
    when (threadscopeExe == Nothing) $ do
        pkgCmd <- systemInstallCmd "threadscope"
        putStrLn $ printf "Hint: install threadscope to aid in profiling/optimization: %s" pkgCmd

    extraRtsArgs <- if any (\case
            ('-':'N':_) -> True
            _           -> False
            ) args
            then pure $ args
            else do
                putStrLn "INFO: as -N<cores> was not provided, -N is being used which uses all cores"
                pure $ "-N":args

    compile scriptName

    let rtsArgs = "+RTS":"-s":extraRtsArgs
    putStrLn $ printf "INFO: using RTS args; %s" (unwords rtsArgs)
    sh $ proc (exeName scriptName) rtsArgs

exeName :: String -> String
exeName scriptName = takeBaseName scriptName ++ exeExtension

noOverwrite :: String -> IO ()
noOverwrite fname = do
    exists <- doesFileExist fname
    when exists . die $ printf "File %s already exists - refusing to overwrite" fname

getRunghcCmdWithReplacement :: String -> String -> IO String
getRunghcCmdWithReplacement scriptName replacement = do
    res <- readRange scriptName "\\{- stack" "-}"
    when (res == Nothing) $ die  "Couldn't find {- stack .* -}"
    let res' = maybe "" id $ res
    pure . unwords . replaceWords "runghc" replacement . words $ res'

getCompileFlags :: String -> IO (Maybe String)
getCompileFlags scriptName = readRange scriptName "\\{- COMPILE_FLAGS" "-}"

-- TODO: used twice to read same file when compiling, should investigate parsers anyway
readRange :: String -> String -> String -> IO (Maybe String)
readRange scriptName reStart reEnd = withFile scriptName ReadMode $ go Nothing
  where
    go :: Maybe String -> Handle -> IO (Maybe String)
    go state handle = do
        line <- hGetLine handle
        if line =~ reStart && line =~ reEnd
            then pure . Just . unwords . dropLast . drop 1 . words $ line
        else if state /= Nothing && line =~ reEnd
            then pure $ (++ (unwords . dropLast $ words line)) <$> state
        else if state /= Nothing
            then go (Just $ (maybe "" id state) ++ line) handle
        else if line =~ reStart
            then go
                (Just $ (maybe "" id state) ++ (unwords . drop 1 $ words line))
                handle
        else
            go state handle

    dropLast strs = take (length strs - 1) strs

replaceWords :: String -> String -> [String] -> [String]
replaceWords from to strs = [if word' == from then to else word' | word' <- strs]

systemInstallCmd :: String -> IO (String)
systemInstallCmd pkg = do
    pkgMan <- foldr f (pure Nothing) ["apt"]
    pure $ case pkgMan of
        Nothing -> printf "use your package manager to install %s" pkg
        Just pm -> printf "%s install %s" pm pkg
  where
    f :: String -> IO (Maybe String) -> IO (Maybe String)
    f curr acc = do
        acc' <- acc
        case acc' of
            Nothing -> (const $ Just curr) <$> findExecutable curr
            Just _  -> acc
