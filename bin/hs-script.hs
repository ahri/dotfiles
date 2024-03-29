#!/usr/bin/env stack
{- stack --resolver lts-12.26 script
    --package containers
    --package process
    --package directory
    --package filepath
    --package regex-posix
    --package neat-interpolation
    --package text
    --package cpphs
    --package haskell-src-exts
    --package safe
    --package http-client
    --package http-client-tls
-}

{- COMPILE_FLAGS -O2 -threaded -rtsopts -eventlog -}

-- Rationale: https://www.ahri.net/practical-haskell-programs-from-scratch/

{-# OPTIONS_GHC -Werror -Wall -Wcompat                                  #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-record-updates   #-}
{-# OPTIONS_GHC -Widentities -Wredundant-constraints                    #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction -Wmissing-home-modules       #-}

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

import Control.Exception
import Control.Monad
import Data.Bits ((.|.))
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Haskell.Exts hiding (parse)
import Language.Preprocessor.Cpphs hiding (Ident)
import NeatInterpolation
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Safe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Info
import System.Process
import Text.Printf
import Text.Regex.Posix

type Resolver = T.Text

defaultResolver :: Resolver
defaultResolver = "lts-13.5"

defaultTemplate :: String
defaultTemplate = "default"

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

templateList :: T.Text
templateList = T.intercalate "|" $ T.pack . fst <$> templates

stackInstallCmd :: Resolver -> String -> String
stackInstallCmd resolver' pkg = printf "stack install --resolver %s %s" (T.unpack resolver') pkg

dependency :: Parse -> String -> IO ()
dependency parse' dep = dependency' parse' dep dep

dependency' :: Parse -> String -> String -> IO ()
dependency' parse' dep pkg = do
    let !resolver' = resolverFromScriptFile parse'
    depExe <- findExecutable dep
    when (isNothing depExe) $
        die $ printf "Error: required dependency missing, install with: %s" (stackInstallCmd resolver' pkg::String)

usage :: IO a
usage = die $ T.unpack [text|
    Usage: script_name command [cmd params]
    Commands:
        new [$templateList]
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
     ) <-  readFile scriptName
       >>= runCpphs defaultCpphsOptions scriptName
       >>= pure . (parseFileContentsWithComments
            $ ParseMode scriptName Haskell2010 [] False False Nothing False)
       >>= pure . fromParseResult

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

resolverFromScriptFile :: Parse -> Resolver
resolverFromScriptFile (_, cs) = if null scriptComments
    then error . printf $ "No '{- stack --resolver RESOLVER' comment found"
    else T.pack $ head scriptComments
  where
    scriptComments = mapMaybe f cs
    f (Comment _ _ s) = if s =~ ("^ ?stack "::String)
        then case words $ (if os == "mingw32" then id else drop 1) s of
            ("stack":"--resolver":x:_) -> Just x
            _                          -> Nothing
        else Nothing

extraCompileFlags :: Parse -> [String]
extraCompileFlags (_, cs) = join $ mapMaybe f cs
  where
    f (Comment _ _ s) = if s =~ ("^ ?COMPILE_FLAGS"::String)
        then Just . drop 1 $ words s
        else Nothing

todos :: Parse -> [String]
todos (_, cs) = mapMaybe f cs
  where
    todoPattern = makeRegexOpts (defaultCompOpt .|. compIgnoreCase) defaultExecOpt ("^ *TODO\\b"::String)

    f (Comment _ _ s) = if match todoPattern s
        then Just . unwords . (["-"] <>) . drop 1 $ words s
        else Nothing

latestResolverLookupOnline :: IO Resolver
latestResolverLookupOnline = do
    putStrLn "Info: attempting to get latest Stack resolver..."
    req <- parseRequest "HEAD https://www.stackage.org/lts"
    manager <- newManager $ managerSetProxy (proxyEnvironment Nothing) tlsManagerSettings

    mRedirects <- catch
        (Just . hrRedirects <$> responseOpenHistory req manager)
        (\(_::SomeException) -> putStrLn "Warning: failed to contact stackage.org" >> pure Nothing)

    maybe
        ((putStrLn $ "Info: failed to get resolver online, using default " <> T.unpack defaultResolver) >> pure defaultResolver)
        (\res -> (putStrLn $ "Info: got resolver " <> T.unpack res) >> pure res)
        $ do
            redirects <- mRedirects
            headers <- responseHeaders . snd <$> headMay redirects
            location <- lookup "Location" headers
            if location =~ ("^/lts-[0-9]+\\.[0-9]+"::String)
                then Just . T.drop 1 $ T.decodeUtf8 location
                else Nothing

new :: FilePath -> [String] -> IO ()
new scriptName cmdArgs = do
    tplName <- case cmdArgs of
        []        -> pure defaultTemplate
        [tplName] -> pure tplName
        _         -> die $ printf "Error: must provide a template - %s" templateList

    noOverwrite scriptName
    resolver' <- latestResolverLookupOnline
    let template = lookup tplName templates
    case template of
        Nothing        -> die $ printf "Error: no template named '%s'" tplName
        Just template' -> writeFile scriptName $ template' resolver'
    perms <- getPermissions scriptName
    setPermissions scriptName $ setOwnerExecutable True perms

repl :: FilePath -> IO ()
repl scriptName = do
    parse' <- parse scriptName
    let exts' = fmap ("-X" <>) $ exts parse'
    let cmd = scriptCmdWithReplacement "exec ghci" parse'
    sh . shell $ printf "%s %s -- %s" cmd scriptName (unwords exts')

watch :: FilePath -> IO ()
watch scriptName = do
    parse' <- parse scriptName
    dependency parse' "ghcid"
    let cmd = scriptCmdWithReplacement "exec ghci" parse'
    sh $ proc "ghcid" ["-c", printf "%s \"%s\"" cmd scriptName]

test :: FilePath -> IO ()
test scriptName = do
    parse' <- parse scriptName
    dependency parse' "ghcid"
    let cmd = scriptCmdWithReplacement "exec ghci" parse'
    sh $ proc "ghcid" ["-c", printf "%s \"%s\"" cmd scriptName, "-T", "tests"]

todo :: FilePath -> IO ()
todo scriptName = do
    todos' <- todos <$> parse scriptName
    traverse_ putStrLn todos'

lint :: FilePath -> IO ()
lint scriptName = do
    parse' <- parse scriptName
    dependency parse' "hlint"
    dependency' parse' "refactor" "apply-refact"
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
    traverse_ removePathForcibly $ (takeBaseName scriptName ++) <$> [".hi", ".dyn_hi", ".o", ".dyn_o"]

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
    resolver' <- resolverFromScriptFile <$> parse scriptName
    hint "threadscope" "to visually represent sparks" systemInstallCmd
    hint "profiteur" "to display cost centres" (pure . stackInstallCmd resolver')
    hint "profiterole" "to concisely reformat a .prof file" (pure . stackInstallCmd resolver')

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

-- TODO: keep track of what resolver was used to test with, in case later
-- versions do not compile, the user could fall back using --tested or something
templates :: [(String, Resolver -> String)]
templates =
    [ (defaultTemplate, \resolver' -> T.unpack [text|
        #!/usr/bin/env stack
        {- stack --resolver $resolver' script
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

        main :: IO ()
        main = do
            putStrLn "Hello world!"
        |])

    , ("shellscript", \resolver' -> T.unpack [text|
        #!/usr/bin/env stack
        {- stack --resolver $resolver' script
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

        -- NB. find most helpful functions in https://hackage.haskell.org/package/directory/docs/System-Directory.html
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

    , ("quickcheck", \resolver' -> T.unpack [text|
        #!/usr/bin/env stack
        {- stack --resolver $resolver' script
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

    , ("httpclient", \resolver' -> T.unpack [text|
        #!/usr/bin/env stack
        {- stack --resolver $resolver' script
            --package aeson
            --package req
            --package data-default
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

        import Control.Monad.IO.Class
        import Data.Default
        import Data.Aeson
        import Network.HTTP.Req

        main :: IO ()
        -- You can either make your monad an instance of 'MonadHttp', or use
        -- 'runReq' in any IO-enabled monad without defining new instances.
        main = runReq def $ do
          let payload = object
                [ "foo" .= (10 :: Int)
                , "bar" .= (20 :: Int) ]
          -- One function—full power and flexibility, automatic retrying on timeouts
          -- and such, automatic connection sharing.
          r <- req POST -- method
            (https "httpbin.org" /: "post") -- safe by construction URL
            (ReqBodyJson payload) -- use built-in options or add your own
            jsonResponse -- specify how to interpret response
            mempty       -- query params, headers, explicit port number, etc.
          liftIO $ print (responseBody r :: Value)
        |])

    , ("httpserver", \resolver' -> T.unpack [text|
        #!/usr/bin/env stack
        {- stack --resolver $resolver' script
            --package bytestring
            --package http-types
            --package warp
            --package wai
            --package blaze-html
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

        import Network.Wai
        import Network.HTTP.Types
        import Network.Wai.Handler.Warp (run)

        import qualified Data.ByteString as B
        import qualified Data.ByteString.Char8 as BC8
        import qualified Data.ByteString.Lazy as BL

        import qualified Text.Blaze.Html5 as H
        import Text.Blaze.Html5 ((!))
        import qualified Text.Blaze.Html5.Attributes as HA
        import qualified Text.Blaze.Html.Renderer.Utf8 as HR

        {- What about...
         -
         - routing?
         -      * there are a number of available routing packages
         -      * a simple applicative parser can suffice, with pattern matching on the
         -        resultant data structure
         -
         - https?
         -      * a more complex subject, and one that is not always needed as a
         -        terminator is often in use - for simple cases use a server like Caddy
         -        to configure letsencrypt and proxy to you
         -}

        app :: Application
        app request respond = do
            BC8.putStrLn $ "Responding to " <> rawReq
            respond $ case rawReq of
                "/"     -> responseLBS
                    status200
                    [("Content-Type", "text/html")]
                    . HR.renderHtml
                    . H.html $ do
                        H.head $ H.title "Hello world!"
                        H.body $ do
                            H.p $ H.text "Hi!"
                            H.p
                                ! HA.class_ "hello"
                                ! HA.onclick "alert('yo')"
                                $ H.text "Click me."

                "/passwd" -> responseFile
                    status200
                    [("Content-Type", "text/plain")]
                    "/etc/passwd"
                    Nothing

                _       -> responseLBS
                    status404
                    [("Content-Type", "text/text")]
                    (BL.fromStrict $ "Couldn't find " <> rawReq)
          where
            rawReq = rawPathInfo request

        main :: IO ()
        main = do
            putStrLn "http://localhost:8080/"
            run 8080 app
        |])

    -- , ("consoleapp", \resolver' -> T.unpack [text|
    --     TODO: brick
    --     |])

    , ("make", \resolver' -> T.unpack [text|
        #!/usr/bin/env stack
        {- stack --resolver $resolver' script
            --package shake
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

        {-# LANGUAGE ScopedTypeVariables, QuasiQuotes, LambdaCase #-}

        import Development.Shake
        import Development.Shake.Command
        import Development.Shake.FilePath
        import Development.Shake.Util

        main :: IO ()
        main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
            want ["_build/run" <.> exe]

            phony "clean" $ do
                putNormal "Cleaning files in _build"
                removeFilesAfter "_build" ["//*"]

            "_build/run" <.> exe %> \out -> do
                cs <- getDirectoryFiles "" ["//*.c"]
                let os = ["_build" </> c -<.> "o" | c <- cs]
                need os
                cmd_ "gcc -o" [out] os

            "_build//*.o" %> \out -> do
                let c = dropDirectory1 $ out -<.> "c"
                let m = out -<.> "m"
                cmd_ "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
                needMakefileDependencies m
        |])

    -- , ("lambda", \resolver' -> T.unpack [text|
    --     TODO: amazon lambda
    --     |])
    
    -- , ("benchmark", \resolver' -> T.unpack [text|
    --     TODO: criterion
    --     |])

    -- , ("diagrams", \resolver' -> T.unpack [text|
    --     TODO: auto-reloading of diagrams would be nice
    --     |])

    -- , ("3layer", \resolver' -> T.unpack [text|
    --     TODO: https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
    --     |])
    ]
