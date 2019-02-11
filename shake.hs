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

{-# LANGUAGE ScopedTypeVariables, QuasiQuotes, LambdaCase, CPP #-}

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

    forM_ scripts $ \script -> case takeExtensions script of
#ifdef mingw32_HOST_OS
        ".cmd" -> binLink script
        ".bat" -> binLink script
        ".exe" -> binLink script
#else
        ""    -> binLink script
        ".sh" -> binLink script
        ".py" -> binLink script
#endif
        ".hs" -> case takeFileName script of
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

        _  -> liftIO . putStrLn $ "Ignoring: " <> script

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

homedir :: IO FilePath
homedir = E.getEnv home
  where
#ifdef mingw32_HOST_OS
    home = "USERPROFILE"
#else
    home = "HOME"
#endif

type FilePathFrom = FilePath
type FilePathTo   = FilePath
linkFile :: FilePathFrom -> FilePathTo -> Action ()
#ifdef mingw32_HOST_OS
linkFile from to = liftIO $ do
    D.removePathForcibly to
    -- TODO: use cmd_ here - it's tricky though because it seems to add quotes, which messes with cmd.exe's weird /C quote rules
    sh . shell $ "cmd.exe /C\"mklink /H \"" <> to <> "\" \"" <> from <> "\"\""

#else
linkFile = liftIO . D.createFileLink
#endif

sh :: CreateProcess -> IO ()
sh cp = do
    exitCode <- withCreateProcess cp (\_ _ _ ph -> waitForProcess ph)
    when (exitCode /= ExitSuccess) $ error $ "Unexpected exit code: " <> show exitCode
    pure ()
