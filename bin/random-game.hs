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
{-# LANGUAGE CPP                                                      #-}

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
    steamPath <- findSteamPath >>= maybe
        (hPutStrLn stderr "Couldn't find Steam" >> exitFailure)
        return

    randAppId <- steamAppIds steamPath >>= randomAppId >>= maybe
        (hPutStrLn stderr "No installed games found" >> exitFailure)
        return

    _ <- openSteamStore steamPath randAppId

    exitSuccess


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

randomAppId :: [SteamAppId] -> IO (Maybe SteamAppId)
randomAppId []     = pure Nothing
randomAppId appIds = Just . (appIds !!) <$> getStdRandom (randomR (0, length appIds - 1))

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

openSteamStore :: SteamDir -> SteamAppId -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
openSteamStore sd (SteamAppId appId) =
    createProcess_ "steam" $ steamProcess sd ["steam://nav/games/details/" ++ appId]
