{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Text.JaTex.Upgrade
  where

import           Control.Lens
import           Control.Monad
import qualified Data.Aeson                         as Aeson
import           Data.Aeson.Lens
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as ByteStringL
import           Data.List
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.IO                       as Text
import           Data.Vector                        (Vector)
import           Distribution.PackageDescription.TH
import           Network.Wreq
import           System.Directory
import           System.Executable.Hash
import           System.Exit
import           System.FilePath
import qualified System.Info
import           System.IO
import           System.IO.Temp
import           System.IO.Unsafe
import           Web.Browser

currentHash :: Maybe ByteString
currentHash = unsafePerformIO $(executableHash)
{-# NOINLINE currentHash #-}

data AppVersion = AppVersion
  { versionName   :: String
  , versionHash   :: Maybe ByteString
  , versionNumber :: String
  }

currentVersion :: AppVersion
currentVersion =
  AppVersion
  { versionHash = currentHash
  , versionName = $(packageVariable (pkgName . package))
  , versionNumber = $(packageVariable (pkgVersion . package))
  }

type VersionList = [(Text, Vector (Text, Text))]

getVersions :: IO VersionList
getVersions = do
  res <-
    get "https://api.github.com/repos/beijaflor-io/jats2tex/releases" >>= asJSON :: IO (Response [Aeson.Value])
  let vs = res ^. responseBody :: [Aeson.Value]
  return $ map parseResponse vs
  where
    parseResponse v =
      ( v ^. key "tag_name" . _String
      , fmap
          (\asset ->
             ( asset ^. key "label" . _String
             , "https://github.com/beijaflor-io/jats2tex/releases/download/" <> v ^. key "name" ._String <>
               "/" <>
               asset ^.
               key "name" .
               _String))
          (v ^. key "assets" . _Array))

isHigherVersionThan :: Text -> Text -> Bool
v1 `isHigherVersionThan` v2 = pv1 > pv2
    where
      pv1 = parseVersion v1
      pv2 = parseVersion v2
      parseVersion :: Text -> (Int, Int, Int)
      parseVersion str | Text.head str == 'v' = parseVersion (Text.tail str)
      parseVersion str = case map (read . Text.unpack) (Text.split (== '.') str) of
          (ma:mi:pa:_) -> (ma, mi, pa)
          (ma:mi:_)    -> (ma, mi, 0)
          (ma:_)       -> (ma, 0, 0)
          _            -> (0, 0, 0)

runUpgrade :: IO ()
runUpgrade = do
  let AppVersion {..} = currentVersion
  putStrLn "Checking GitHub for updates..."
  vs <- getVersions
  case find
         (\(v, _) -> v `isHigherVersionThan` Text.pack versionNumber)
         vs of
    Just (v, assets) -> do
      Text.putStrLn $ "Found new version " <> v
      putStrLn "Deciding package to download"
      packageFormat <- getPackageFormat
      case find
             (\(k, _) -> packageFormatSuffix packageFormat `Text.isSuffixOf` k)
             assets of
        Just (k, u) -> do
          putStrLn $ "Downloading package at " <> show u <> "..."
          fp <- do
              temporaryDir <- getTemporaryDirectory >>= \t -> createTempDirectory t "jats2tex-upgrade"
              res <- get (Text.unpack u)
              let b = res ^. responseBody
                  fp = temporaryDir </> Text.unpack k
              putStrLn "To:"
              putStrLn fp
              createDirectoryIfMissing True (takeDirectory fp)
              ByteStringL.writeFile fp b
              return fp
          putStrLn "Downloaded."
          a <- askQuestion "Open package" ["Yes", "No", "Open package directory"]
          case a of
              "Yes" -> void $ openBrowser fp
              "No"  -> return ()
              "Open package directory" -> void $ openBrowser (takeDirectory fp)
              _     -> error "Impossible"
        Nothing -> do
          Text.hPutStrLn stderr ("No package found for " <> v)
          exitWith (ExitFailure 1)
    Nothing -> do
      putStrLn "Already running at latest version"
      putVersionInfo

getPackageFormat :: IO PackageFormat
getPackageFormat =
  case [System.Info.os, System.Info.arch] of
    [_, a]
      | a /= "x86_64" -> do
        hPutStrLn stderr "Only 64-bits platforms are supported at this time."
        exitWith (ExitFailure 1)
    ["darwin", "x86_64"] -> do
        a <- askQuestion "Installation Method" ["Download PKG", "Download Binary"]
        case a of
            "Download PKG"    -> return OsxPackage
            "Download Binary" -> return OsxBinaryPackage
            _                 -> error "Impossible"
    ["linux", "x86_64"] -> do
        a <- askQuestion "Installation Method" ["Download RPM", "Download Deb", "Download Binary"]
        case a of
            "Download Deb"    -> return DebPackage
            "Download RPM"    -> return RpmPackage
            "Download Binary" -> return LinuxBinaryPackage
            _                 -> error "Impossible"
    _ -> do
        hPutStrLn stderr "Your platform does not support automatic upgrade yet."
        exitWith (ExitFailure 1)

askQuestion :: String -> [String] -> IO String
askQuestion question os = do
  forM_ (zip os ([1 ..] :: [Int])) $ \(option, n) ->
    putStrLn $ "  " <> show n <> ") " <> option
  loop
  where
    lenQs = length os
    loop = do
      putStr $ question <> ": "
      hFlush stdout
      inp <- Text.unpack . Text.strip . Text.pack <$> getLine
      case inp of
        "" -> loop
        _ ->
          let ar = read inp - 1 :: Int
          in if ar < 0 || ar >= lenQs
               then hPutStrLn stderr "Invalid choice" >> loop
               else return (os !! ar)


data PackageFormat = RpmPackage
                   | DebPackage
                   | OsxPackage
                   | OsxBinaryPackage
                   | LinuxBinaryPackage

packageFormatSuffix :: PackageFormat -> Text
packageFormatSuffix RpmPackage         = ".x86_64.rpm"
packageFormatSuffix OsxBinaryPackage   = "-darwin.tar.gz"
packageFormatSuffix LinuxBinaryPackage = "-linux_amd64.tar.gz"
packageFormatSuffix OsxPackage         = ".pkg"
packageFormatSuffix DebPackage         = ".deb"

putVersionInfo :: IO ()
putVersionInfo = do
  let AppVersion {..} = currentVersion
  putStrLn $ versionName <> "@" <> versionNumber
