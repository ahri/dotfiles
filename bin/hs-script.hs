#!/usr/bin/env stack
{- stack --resolver lts-13.4 script
    --package containers
    --package process
    --package directory
    --package filepath
    --package regex-posix
    --package neat-interpolation
    --package text
    --package haskell-src-exts
-}

{- COMPILE_FLAGS -O2 -threaded -rtsopts -eventlog -}

-- Rationale: https://www.ahri.net/practical-haskell-programs-from-scratch/

{-# OPTIONS_GHC -Werror -Wall -Wcompat                                  #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-record-updates   #-}
{-# OPTIONS_GHC -Widentities -Wredundant-constraints                    #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction -Wmissing-home-modules       #-}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import Language.Haskell.Exts hiding (parse)
import NeatInterpolation
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Error
import System.Process
import Text.Printf
import Text.Regex.Posix

resolver :: T.Text
resolver = "lts-13.4"

templateList :: T.Text
templateList = T.intercalate "|" $ T.pack . fst <$> templates

main :: IO ()
main = do
    (scriptPath, command, cmdArgs) <- getArgs >>= processArgs
    setCurrentDirectory $ takeDirectory scriptPath
    let scriptName = takeFileName scriptPath

    case command of
        "new"         -> new scriptName cmdArgs
        "repl"        -> repl scriptName
        "watch"       -> watch scriptName
        "test"        -> test scriptName
        "lint"        -> lint scriptName
        "todo"        -> todo scriptName
        "compile"     -> compile scriptName []
        "profile"     -> profile scriptName cmdArgs
        _             -> die $ printf "Error: unknown command: %s" command

type Command = String
processArgs :: [String] -> IO (FilePath, Command, [String])
processArgs (scriptPath:command:cmdArgs) = pure (scriptPath, command, cmdArgs)
processArgs _                            = usage

usage :: IO a
usage = die $ T.unpack [text|
    Usage: script_name command [cmd params]
    Commands:
        new $templateList
        repl
        watch
        test
        lint
        todo
        compile
        profile [additional RTS options]
    |]

sh :: CreateProcess -> IO ()
sh cp = do
    exitCode <- withCreateProcess cp (\_ _ _ ph -> waitForProcess ph)
    when (exitCode /= ExitSuccess) $ exitWith exitCode
    pure ()

type Parse = ([ModulePragma ()], [Comment])
parse :: FilePath -> IO Parse
parse scriptName = do
    ( Module
        _
        _head
        pragmas
        _imports
        _declarations
     , comments
     , unknownPragmas
     ) <- fromParseResult <$> parseFileWithCommentsAndPragmas
        (ParseMode scriptName Haskell2010 [] False False Nothing False)
        scriptName

    when (not . null $ unknownPragmas)
        . die $ printf "Error: unknown pragmas: %s" (show unknownPragmas)

    pure ((() <$) <$> pragmas, comments)

exts :: Parse -> [String]
exts (ps, _) = join $ mapMaybe f ps
  where
    f (LanguagePragma _ xs) = Just $ mapMaybe g xs
    f _                     = Nothing

    g (Ident _ s) = Just s
    g _           = Nothing

scriptCmdWithReplacement :: String -> Parse -> String
scriptCmdWithReplacement replacement (_, cs) = if null scriptComments
    then error . printf $ "No '{- stack' comment found"
    else head scriptComments
  where
    scriptComments = mapMaybe f cs
    f (Comment _ _ s) = if s =~ ("^ ?stack "::String)
        then Just
            . unwords
            . fmap
                ( replaceWord "runghc" replacement
                . replaceWord "script" replacement)
            . words
            $ s
        else Nothing

    replaceWord from to word = if word == from then to else word

extraCompileFlags :: Parse -> [String]
extraCompileFlags (_, cs) = join $ mapMaybe f cs
  where
    f (Comment _ _ s) = if s =~ ("^ ?COMPILE_FLAGS"::String)
        then Just . drop 1 $ words s
        else Nothing

todos :: Parse -> [String]
todos (_, cs) = mapMaybe f cs
  where
    f (Comment _ _ s) = if s =~ ("^ ?TODO\\b"::String)
        then Just . unwords . (["-"] <>) . drop 1 $ words s
        else Nothing

new :: FilePath -> [String] -> IO ()
new scriptName cmdArgs = do
    case cmdArgs of
        [tplName] -> do
            noOverwrite scriptName
            let template = lookup tplName templates
            case template of
                Nothing        -> die $ printf "Error: no template named '%s'" tplName
                Just template' -> writeFile scriptName template'
            perms <- getPermissions scriptName
            setPermissions scriptName $ setOwnerExecutable True perms

        _         -> die $ printf "Error: must provide a template - %s" templateList

repl :: FilePath -> IO ()
repl scriptName = do
    parse' <- parse scriptName
    let exts' = fmap ("-X" <>) $ exts parse'
    let cmd = scriptCmdWithReplacement "exec ghci" parse'
    sh . shell $ printf "%s %s -- %s" cmd scriptName (unwords exts')

watch :: FilePath -> IO ()
watch scriptName = do
    dependency "ghcid"
    cmd <- scriptCmdWithReplacement "exec ghci" <$> parse scriptName
    sh $ proc "ghcid" ["-c", printf "%s \"%s\"" cmd scriptName]

test :: FilePath -> IO ()
test scriptName = do
    dependency "ghcid"
    cmd <- scriptCmdWithReplacement "exec ghci" <$> parse scriptName
    sh $ proc "ghcid" ["-c", printf "%s \"%s\"" cmd scriptName, "-T", "tests"]

todo :: FilePath -> IO ()
todo scriptName = do
    todos' <- todos <$> parse scriptName
    traverse_ putStrLn todos'

lint :: FilePath -> IO ()
lint scriptName = do
    dependency "hlint"
    dependency' "refactor" "apply-refact"
    sh $ proc "hlint" ["--refactor", "--refactor-options=-is", scriptName]

compile :: FilePath -> [String] -> IO ()
compile scriptName cmdlineExtraCompileFlags = do
    parse' <- parse scriptName
    let cmd = scriptCmdWithReplacement "exec ghc" parse'
    let flags = unwords $ extraCompileFlags parse' <> cmdlineExtraCompileFlags
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
dependency dep = dependency' dep dep

dependency' :: String -> String -> IO ()
dependency' dep pkg = do
    depExe <- findExecutable dep
    when (isNothing depExe) $
        die $ printf "Error: required dependency missing, install with: %s" (stackInstallCmd pkg)

stackInstallCmd :: String -> String
stackInstallCmd pkg = printf "stack install --resolver %s %s" (T.unpack resolver) pkg

rmF :: FilePath -> IO ()
rmF fname = removeFile fname `catch` handleErrs
  where
    handleErrs e
        | isDoesNotExistError e = pure ()
        | otherwise = throwIO e

templates :: [(String, String)]
templates =
    [ ("termapp", T.unpack [text|
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
        |])

    , ("quickcheck", T.unpack [text|
        #!/usr/bin/env stack
        {- stack --resolver $resolver script
            --package QuickCheck
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

        {-# LANGUAGE ScopedTypeVariables #-}

        import Test.QuickCheck

        tests :: IO ()
        tests = quickCheck ((==)::Int -> Int -> Bool)
        |])

    -- , ("3layer", T.unpack [text|
    --     TODO
    --     |])
    ]
