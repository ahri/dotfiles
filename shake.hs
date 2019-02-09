#!/usr/bin/env stack
{- stack --resolver lts-12.26 script
   --package shake
   --package directory
   --package executable-path
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
import qualified System.Directory as D

type FromFilePath = FilePath
type ToFilePath = FilePath

-- TODO: aim to remove 'directory' package in favour of idiomatic Shake stuff

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    home <- liftIO homedir

    root <- liftIO
        $   getScriptPath -- NB. if bugs are found, try FindBin package
        >>= \case
            Executable s -> pure $ takeDirectory s
            RunGHC s     -> pure $ takeDirectory s
            Interactive  -> D.getCurrentDirectory
        >>= \d -> D.setCurrentDirectory d >> pure d

    hsScript <- liftIO
        $   D.findExecutable "hs-script"
        >>= pure . maybe (root </> "bin" </> "hs-script.hs") id

    scripts <- liftIO
        $   D.listDirectory "bin"
        >>= pure . fmap ((root </> "bin") </>)
        >>= filterM D.doesFileExist

    let basicLink script = do
            let target = home </> "bin" </> takeFileName script
            want [target]
            target %> \_ -> do
                need [script]
                link script target

    forM_ scripts $ \script -> case takeExtensions script of
        ""    -> basicLink script
        ".sh" -> basicLink script
        ".py" -> basicLink script
        ".hs" -> do
            let target = home </> "bin" </> takeFileName script -<.> exe
            want [target]
            target %> \_ -> do
                need [script]
                cmd_ [hsScript, script, "compile"]
                liftIO $ D.renameFile (script -<.> exe) target

        _  -> error $ "Unexpected extension: " <> script

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

link :: FromFilePath -> ToFilePath -> Action ()
#ifdef mingw32_HOST_OS
link from to = do
    cmd_ "del /f/q" [to]
    cmd_ "cmd /C mklink /H" [to, from]
#else
link from to = do
    cmd_ "rm -f" [to]
    cmd_ "ln -s" [from, to]
#endif
