#!/usr/bin/env stack
{- stack --resolver lts-13.4 script
    --package containers
    --package process
    --package directory
    --package filepath
    --package regex-posix
    --package neat-interpolation
    --package text
-}

{- COMPILE_FLAGS -O2 -threaded -rtsopts -eventlog -}

-- Rationale: https://www.ahri.net/practical-haskell-programs-from-scratch/

{-# OPTIONS_GHC -Werror -Wall -Wcompat                                  #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-record-updates   #-}
{-# OPTIONS_GHC -Widentities -Wredundant-constraints                    #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction -Wmissing-home-modules       #-}

-- TODO: find a neater way to deal with args so I can remove this
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns                       #-}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.List
import qualified Data.Text as T
import NeatInterpolation
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Process
import Text.Printf
import Text.Regex.Posix

resolver :: T.Text
resolver = "lts-13.4"

template :: String
template = T.unpack [text|
	#!/usr/bin/env stack
	{- stack --resolver $resolver script
	    --package containers
	    --package process
	    --package directory
	    --package filepath
	    --package regex-posix
	-}

	{- COMPILE_FLAGS -O2 -threaded -rtsopts -eventlog -}

	-- https://downloads.haskell.org/~ghc/8.6.3/docs/html/users_guide/using-warnings.html
	{-# OPTIONS_GHC -Werror -Wall -Wcompat                                  #-}
	{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-record-updates   #-}
	{-# OPTIONS_GHC -Widentities -Wredundant-constraints                    #-}
	{-# OPTIONS_GHC -Wmonomorphism-restriction -Wmissing-home-modules       #-}

    -- The idea is to remove these when you want to tidy your code up
	{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches       #-}
	{-# OPTIONS_GHC -fno-warn-unused-top-binds -fno-warn-unused-local-binds #-}
    -- and add this, also when wanting to clean up code
	-- {-# OPTIONS_GHC -ddump-minimal-imports                               #-}

    {-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}

    import Control.Monad
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
	import Text.Regex.Posix

	main :: IO ()
	main = do
        sh $ shell "echo foo"
        sh $ proc "echo" ["bar"]
        findExecutable "stack" >>= print

		args <- getArgs
		putStrLn "args:"
		traverse_ (hPutStrLn stderr) args 

		putStrLn "stdin:"
		getContents >>= putStrLn

		exitFailure

    sh :: CreateProcess -> IO ()
    sh cp = do
        exitCode <- withCreateProcess cp (\_ _ _ ph -> waitForProcess ph)
        when (exitCode /= ExitSuccess) $ exitWith exitCode
        pure ()
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
        "new"         -> new scriptName -- TODO: it would be nice to have templates: termapp, test, quickcheck
        "repl"        -> repl scriptName
        "watch"       -> watch scriptName
        "test"        -> test scriptName
        "lint"        -> lint scriptName
        "compile"     -> compile scriptName []
        "profile"     -> profile scriptName cmdArgs
        _             -> die $ printf "Error: unknown command: %s" command

usage :: IO ()
usage = die $ T.unpack [text|
    Usage: script_name command [cmd params]
    Commands:
        new
        repl
        watch
        test
        lint
        compile
        profile [additional RTS options]
    |]

sh :: CreateProcess -> IO ()
sh cp = do
    exitCode <- withCreateProcess cp (\_ _ _ ph -> waitForProcess ph)
    when (exitCode /= ExitSuccess) $ exitWith exitCode
    pure ()

new :: FilePath -> IO ()
new scriptName = do
    noOverwrite scriptName
    writeFile scriptName template
    perms <- getPermissions scriptName
    setPermissions scriptName $ setOwnerExecutable True perms

repl :: FilePath -> IO ()
repl scriptName = do
    -- TODO: reading the file twice here
    exts <- (fmap.fmap) ("-X" <>) $ getLangExts scriptName
    cmd <- getScriptCmdWithReplacement scriptName "exec ghci"
    sh . shell $ printf "%s %s -- %s" cmd scriptName (intercalate " " exts)

watch :: FilePath -> IO ()
watch scriptName = do
    dependency "ghcid"
    cmd <- getScriptCmdWithReplacement scriptName "exec ghci"
    sh $ proc "ghcid" ["-c", printf "%s \"%s\"" cmd scriptName]

test :: FilePath -> IO ()
test scriptName = do
    dependency "ghcid"
    cmd <- getScriptCmdWithReplacement scriptName "exec ghci"
    sh $ proc "ghcid" ["-c", printf "%s \"%s\"" cmd scriptName, "-T", "tests"]

lint :: FilePath -> IO ()
lint scriptName = do
    dependency "hlint"
    dependency "apply-refact"
    sh $ proc "hlint" ["--refactor", "--refactor-options=-is", scriptName]

compile :: FilePath -> [String] -> IO ()
compile scriptName extraCompileFlags = do
    cmd <- getScriptCmdWithReplacement scriptName "exec ghc"
    flags :: String <- unwords . (++ extraCompileFlags) . replaceWords "COMPILE_FLAGS" "" . words . maybe "" id <$> getCompileFlags scriptName
    noOverwrite $ exeName scriptName
    let fullCmd :: String = printf "%s -- %s %s" cmd flags scriptName
    printf "Info: compile command: %s\n" fullCmd
    sh . shell $ fullCmd
    traverse_ rmF $ (takeBaseName scriptName ++) <$> [".hi", ".dyn_hi", ".o", ".dyn_o"]

hint :: String -> String -> (String -> IO String) -> IO ()
hint exeN descr f = do
    exe <- findExecutable exeN
    case exe of
        Nothing -> do
            pkgCmd <- f exeN
            printf "Hint: install %s %s: %s\n" exeN descr pkgCmd
        _ -> printf "Use %s %s\n" exeN descr

profile :: FilePath -> [String] -> IO ()
profile scriptName args = do
    hint "threadscope" "to visually represent sparks" systemInstallCmd
    hint "profiteur" "to display cost centres" (pure . stackInstallCmd)
    hint "profiterole" "to concisely reformat a .prof file" (pure . stackInstallCmd)

    -- TODO: https://stackoverflow.com/questions/32123475/profiling-builds-with-stack

    compile scriptName ["-prof", "-fprof-auto"]

    extraRtsArgs <- if any (\case
            ('-':'N':_) -> True
            _           -> False
            ) args
            then pure args
            else do
                putStrLn "Info: as -N<cores> was not provided, -N is being used which uses all cores"
                pure $ "-N":args

    let rtsArgs = "+RTS":"-s":extraRtsArgs
    printf "Info: using RTS args; %s\n" (unwords rtsArgs)
    sh $ proc (exeName scriptName) rtsArgs

exeName :: FilePath -> String
exeName scriptName = takeBaseName scriptName ++ exeExtension

noOverwrite :: FilePath -> IO ()
noOverwrite fname = do
    exists <- doesFileExist fname
    when exists . die $ printf "Error: file %s already exists - refusing to overwrite" fname

getScriptCmdWithReplacement :: FilePath -> String -> IO String
getScriptCmdWithReplacement scriptName replacement = do
    res <- readRange scriptName "\\{- stack" "-}"
    when (res == Nothing) $ die "Error: couldn't find {- stack .* -}"
    let res' = maybe "" id res
    pure
        . unwords
        . replaceWords "runghc" replacement
        . replaceWords "script" replacement
        . words
        $ res'

getCompileFlags :: FilePath -> IO (Maybe String)
getCompileFlags scriptName = readRange scriptName "\\{- COMPILE_FLAGS" "-}"

getLangExts :: FilePath -> IO [String]
getLangExts scriptName = do
    header <- readRange scriptName "^\\{- stack" "^import"
    let hw = maybe [] words header
    pure . snd $ foldl f (False, []) hw
  where
    f (False, xs) "LANGUAGE" = (True,  xs)
    f (True, xs)  "#-}"      = (False, xs)
    f (True, xs)  x          = (True, (filter (/= ',') x):xs)
    f acc         _          = acc

type RegexStr = String

-- TODO: used twice to read same file when compiling, should investigate parsers anyway
readRange :: FilePath -> RegexStr -> RegexStr -> IO (Maybe String)
readRange scriptName start end = withFile scriptName ReadMode $ go Nothing
  where
    go :: Maybe String -> Handle -> IO (Maybe String)
    go state fp = do
        line <- (<> " ") <$> hGetLine fp
        if line =~ start && line =~ end
            then pure . Just . unwords . dropLast . drop 1 . words $ line
        else if state /= Nothing && line =~ end
            then pure $ (<> (unwords . dropLast $ words line)) <$> state
        else if state /= Nothing
            then go (Just $ (maybe "" id state) ++ line) fp
        else if line =~ start
            then go
                (Just $ (maybe "" id state) ++ (unwords . drop 1 $ words line))
                fp
        else
            go state fp

    dropLast strs = take (length strs - 1) strs

replaceWords :: String -> String -> [String] -> [String]
replaceWords from to strs = [if word' == from then to else word' | word' <- strs]

systemInstallCmd :: String -> IO String
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
            Nothing -> const (Just curr) <$> findExecutable curr
            Just _  -> acc

dependency :: String -> IO ()
dependency dep = do
    depExe <- findExecutable dep
    when (depExe == Nothing) $ do
        die $ printf "Error: required dependency missing, install with: %s" (stackInstallCmd dep)

stackInstallCmd :: String -> String
stackInstallCmd pkg = printf "stack install --resolver %s %s" (T.unpack resolver) pkg

rmF :: FilePath -> IO ()
rmF fname = removeFile fname `catch` handleErrs
  where
    handleErrs e
        | isDoesNotExistError e = pure ()
        | otherwise = throwIO e
