#!/usr/bin/env stack
{- stack --resolver lts-12.26 script
   --package regex-compat
   --package process
   --package random
-}

-- https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/using-warnings.html
{-# OPTIONS_GHC -Werror -Wall -Wcompat                                #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Widentities -Wredundant-constraints                  #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction -Wmissing-home-modules     #-}
-- {-# OPTIONS_GHC -ddump-minimal-imports                             #-}
{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase                     #-}

import             Control.Monad
import             Data.List
import             Data.Maybe
import             System.Directory
import             System.Exit
import             System.IO
import             System.Process
import             System.Random
import             Text.Regex

#ifdef mingw32_HOST_OS
import             Control.Exception
import             System.Win32.Registry
#else
import             System.Environment
#endif

newtype SteamDir = SteamDir FilePath deriving (Show, Eq)

newtype SteamAppId = SteamAppId String deriving (Show, Eq)

main :: IO ()
main = do
    allGames <- (<>) <$> steamGames <*> gogGames
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

findSteamPath :: IO (Maybe SteamDir)
#ifdef mingw32_HOST_OS
findSteamPath = do
    vals <- bracket
        (regOpenKey hKEY_CURRENT_USER "Software\\Valve\\Steam")
        regCloseKey
        regEnumKeyVals
    
    pure $ foldr f Nothing vals
      where
        f cur acc = case cur of
            ("SteamPath", path, _) -> Just $ SteamDir path
            _                      -> acc
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

            pure $ executeGame
                <$> (lookupRegVal "WORKINGDIR" vals)
                <*> (lookupRegVal "LAUNCHCOMMAND" vals)
    )
  where    
    executeGame :: FilePath -> String -> IO ()
    executeGame wd cmd = do
        putStrLn $ "Running " <> cmd
        setCurrentDirectory wd
        _ <- createProcess_ "gog" . shell $ if ['"'] `isPrefixOf` cmd
            then cmd
            else ['"'] <> cmd <> ['"']
        pure ()

#else
gogGames = undefined
#endif

lookupRegVal :: Eq a => a -> [(a, b, c)] -> Maybe b
lookupRegVal k = \case
    []     -> Nothing
    ((k', v, _):xs) -> if k == k'
        then Just v
        else lookupRegVal k xs
