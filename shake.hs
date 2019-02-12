#!/usr/bin/env stack
{- stack --resolver lts-12.26 script
    --package shake
    --package directory
    --package executable-path
    --package process
-}

{- COMPILE_FLAGS -O2 -rtsopts -threaded -with-rtsopts=-I0 -}

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

{-# LANGUAGE ScopedTypeVariables, QuasiQuotes, LambdaCase #-}

import Control.Monad
import Data.Maybe
import Data.List
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Environment.Executable
import qualified System.Environment as E
import System.Exit
import qualified System.Directory as D
import qualified System.Info
import System.Process

type FromFilePath = FilePath
type ToFilePath = FilePath

-- TODO: aim to remove 'directory' package in favour of idiomatic Shake stuff

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    home <- liftIO homedir

    root <- liftIO
        $   getScriptPath
        >>= \case
            Executable s -> pure $ takeDirectory s
            RunGHC s     -> pure $ takeDirectory s
            Interactive  -> D.getCurrentDirectory
        >>= \d -> D.setCurrentDirectory d >> pure d

    scripts <- liftIO
        $   D.listDirectory "bin"
        >>= pure . fmap ((root </> "bin") </>)
        >>= filterM D.doesFileExist

    let binLink script = do
            let target = home </> "bin" </> takeFileName script
            want [target]
            target %> \_ -> do
                need [script]
                linkFile script target

    let hsScript = home </> "bin" </> "hs-script" <.> exe

    let ignore script = liftIO . putStrLn $ "Ignoring: " <> script

    let compileHs script = case takeFileName script of
            "hs-script.hs" -> do -- NB. special case as it needs bootstrapping
                let target = home </> "bin" </> takeFileName script -<.> exe
                want [target]
                target %> \_ -> do
                    need [script]
                    cmd_ ["stack", script, script, "compile"]
                    liftIO $ D.renameFile (script -<.> exe) target

            _              -> do
                let target = home </> "bin" </> takeFileName script -<.> exe
                want [target]
                target %> \_ -> do
                    need [hsScript, script]
                    cmd_ [hsScript, script, "compile"]
                    liftIO $ D.renameFile (script -<.> exe) target

    forM_ scripts $ \script -> if isWindows
        then case takeExtensions script of
            ".cmd" -> binLink script
            ".bat" -> binLink script
            ".exe" -> binLink script
            ".hs"  -> compileHs script
            _      -> ignore script
        else case takeExtensions script of
            ""     -> binLink script
            ".sh"  -> binLink script
            ".py"  -> binLink script
            ".hs"  -> compileHs script
            _      -> ignore script

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

homedir :: IO FilePath
homedir = E.getEnv home
  where home = if isWindows
        then "USERPROFILE"
        else "HOME"

type FilePathFrom = FilePath
type FilePathTo   = FilePath
linkFile :: FilePathFrom -> FilePathTo -> Action ()
linkFile from to = liftIO $ if isWindows
    then do
        D.removePathForcibly to
        -- TODO: use cmd_ here - it's tricky though because it seems to add quotes, which messes with cmd.exe's weird /C quote rules
        -- TODO: by using "shell" is "cmd.exe" being called in a nested manner here?
        sh . shell $ "cmd.exe /C\"mklink /H \"" <> to <> "\" \"" <> from <> "\"\""
    else do
        D.removePathForcibly to
        D.createFileLink from to

sh :: CreateProcess -> IO ()
sh cp = do
    exitCode <- withCreateProcess cp (\_ _ _ ph -> waitForProcess ph)
    when (exitCode /= ExitSuccess) $ error $ "Unexpected exit code: " <> show exitCode
    pure ()

isWindows :: Bool
isWindows = "mingw" `isPrefixOf` System.Info.os
