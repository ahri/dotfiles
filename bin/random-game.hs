#!/usr/bin/env stack
{- stack --resolver lts-12.26 script
   --package regex-compat
   --package process
   --package random
   --package filepath
   --package aeson
   --package bytestring
-}

-- https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/using-warnings.html
{-# OPTIONS_GHC -Werror -Wall -Wcompat                                #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Widentities -Wredundant-constraints                  #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction -Wmissing-home-modules     #-}
-- {-# OPTIONS_GHC -ddump-minimal-imports                             #-}
{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase, OverloadedStrings  #-}

import           Control.Monad
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           System.Random
import           Text.Regex

#ifdef mingw32_HOST_OS
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import           Data.List
import           Data.Maybe
import           Control.Exception
import           System.FilePath
import           System.Win32.Registry
#endif

newtype SteamDir = SteamDir FilePath deriving (Show, Eq)

newtype SteamAppId = SteamAppId String deriving (Show, Eq)

libraries :: IO [(String, [IO ()])]
libraries =
    (\steam gog epic ->
        [ ("steam", steam)
        , ("gog",   gog)
        , ("epic",  epic)
        ]
    )
    <$> steamGames
    <*> gogGames
    <*> epicGames

main :: IO ()
main = do
    args <- getArgs
    allGames :: [IO ()] <- case args of
        [] -> fmap join $ (fmap . fmap) snd libraries
        _  -> fmap concat $ (flip traverse) args $ \arg ->
            fmap (maybe [] id . lookup arg) libraries

    join . maybe
        (hPutStrLn stderr "No installed games found" >> exitFailure)
        id
        $ pick allGames

    exitSuccess


pick :: [a] -> Maybe (IO a)
pick = \case
    [] -> Nothing
    xs -> Just $ fmap (xs !!) $ getStdRandom (randomR (0, length xs - 1))

steamGames :: IO [IO ()]
steamGames = do
    mSteamPath <- findSteamPath
    case mSteamPath of
        Nothing        -> pure []
        Just steamPath -> (fmap.fmap) (openSteamStore steamPath) $ steamAppIds steamPath

  where
    findSteamPath :: IO (Maybe SteamDir)
#ifdef mingw32_HOST_OS
    findSteamPath = do
        vals <- bracket
            (regOpenKey hKEY_CURRENT_USER "Software\\Valve\\Steam")
            regCloseKey
            regEnumKeyVals
        
        pure $ fmap SteamDir $ lookupRegVal "SteamPath" vals
#else
    findSteamPath = Just . SteamDir . (++ "/.steam/steam") <$> getEnv "HOME"
#endif

    steamAppIds :: SteamDir -> IO [SteamAppId]
    steamAppIds (SteamDir sd) = filterSteamManifestsForIdentifiers <$> listDirectory (sd ++ "/steamapps")
      where
        filterSteamManifestsForIdentifiers :: [FilePath] -> [SteamAppId]
        filterSteamManifestsForIdentifiers = foldr f []

        f cur acc = case matchRegex manifestPattern cur of
            Just [appId] -> SteamAppId appId : acc
            _            -> acc

        manifestPattern :: Regex
        manifestPattern = mkRegex "^appmanifest_([0-9]+)\\.acf$"

    steamProcess :: SteamDir -> [String] -> CreateProcess
#ifdef mingw32_HOST_OS
    steamProcess (SteamDir sd) params = proc
        (sd ++ "/Steam.exe")
        params
#else
    steamProcess _ params = proc
        "/bin/bash"
        ("/usr/bin/steam" : params)
#endif

    openSteamStore :: SteamDir -> SteamAppId -> IO ()
    openSteamStore sd (SteamAppId appId) = do
        putStrLn $ "Opening Steam app for ID " <> appId
        _ <- createProcess_ "steam" $ steamProcess sd ["steam://nav/games/details/" ++ appId]
        pure ()

gogGames :: IO [IO ()]
#ifdef mingw32_HOST_OS
gogGames = bracket
    (regOpenKey hKEY_LOCAL_MACHINE "Software\\Wow6432Node\\GOG.com\\Games")
    regCloseKey
    (\gamesKey -> do
        gameIds <- regEnumKeys gamesKey
        fmap catMaybes $ (flip traverse) gameIds $ \gameId -> do
            vals <- bracket
                (regOpenKey gamesKey gameId)
                regCloseKey
                regEnumKeyVals

            pure $ execute "gog"
                <$> (lookupRegVal "WORKINGDIR" vals)
                <*> (lookupRegVal "EXE" vals)
                <*> (lookupRegVal "LAUNCHPARAM" vals)
    )

#else
gogGames = undefined
#endif

epicGames :: IO [IO ()]
#ifdef mingw32_HOST_OS
epicGames = do
    vals <- bracket 
        (regOpenKey hKEY_LOCAL_MACHINE "Software\\Wow6432Node\\Epic Games\\EpicGamesLauncher")
        regCloseKey
        regEnumKeyVals

    case lookupRegVal "AppDataPath" vals of
        Nothing -> pure mempty
        Just dir -> do
            let manifestsDir = joinPath [dir, "Manifests"]
            exists <- doesDirectoryExist manifestsDir
            if not exists
                then pure []
                else listDirectory manifestsDir
                    >>= (pure . (fmap $ \manifest -> joinPath [manifestsDir, manifest]) . filter (".item" `isSuffixOf`))
                    >>= traverse LBS.readFile
                    >>= (pure . \jsons -> do
                            j <- jsons
                            let mp = do
                                (wd, exe, args) <- decode j
                                    >>= (parseMaybe $ withObject "EpicJsonManifest" $ \v -> (,,)
                                            <$> v .: "InstallLocation"
                                            <*> v .: "LaunchExecutable"
                                            <*> v .: "LaunchCommand"
                                        )

                                pure $ execute "epic" wd exe args

                            case mp of
                                Nothing -> mempty
                                Just p  -> pure p
                        )
#else
epicGames = undefined
#endif

#ifdef mingw32_HOST_OS
lookupRegVal :: Eq a => a -> [(a, b, c)] -> Maybe b
lookupRegVal k = \case
    []     -> Nothing
    ((k', v, _):xs) -> if k == k'
        then Just v
        else lookupRegVal k xs

execute :: String -> FilePath -> FilePath -> String -> IO ()
execute name wd exe args = do
    setCurrentDirectory wd
    let exe' = if (not $ ['"'] `isPrefixOf` exe) && ' ' `elem` exe
        then ['"'] <> exe <> ['"']
        else exe
    let cmd = exe' <> " " <> args
    putStrLn $ "Running " <> cmd <> " in " <> wd
    _ <- createProcess_ name $ shell cmd
    pure ()
#endif
