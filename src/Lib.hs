{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}


module Lib
    ( someFunc
    ) where


import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSL
import qualified Data.Yaml as Y
import Data.Yaml
    ( FromJSON(..)
    , (.:)
    , (.:?) )
import Text.RawString.QQ
import Control.Applicative
import Data.Attoparsec.Text
import Data.Attoparsec.Text as AT
import Text.Regex (mkRegex, subRegex)
import Data.Maybe (fromJust)


sampleYaml :: ByteString
sampleYaml = [r|
resolver: lts-6.5
packages:
  - location: some/location
  - location:
      hg: lolol
  - location:
      git: git@github.com
      commit: 68ec213
flags: {}
|]


someFunc :: IO ()
someFunc = do
  print $ (Y.decodeEither sampleYaml :: Either String Config)


someFunc' :: IO ()
someFunc' = do
  contents <- BSL.unpack <$> readStackConfig "./sample-stack.yaml"
  e <- (Y.decodeEither <$> readStackConfig "./sample-stack.yaml") :: IO (Either String Config)
  case e of
    Left _ -> putStrLn "parser error"
    Right (Config packages) -> do
      let commits = filter (/=Nothing) $ fmap (\p -> extractCommits p) packages
          regexes = mkRegex . T.unpack . fromJust <$> commits
          newContent = foldl (\content rgx -> subRegex rgx content "newcommit") contents regexes
      print newContent
      print commits
      return ()
  where
    extractCommits p = case location p of
                         RemoteLocation remote -> commit remote
                         _ -> mempty


readStackConfig :: FilePath
                -> IO ByteString
readStackConfig path = do
  contents <- BS.readFile path
  return contents


parseGitUrl :: Text -> Either String GitDetails
parseGitUrl url = flip parseOnly url $ choice [parseGitHttps, parseGitSsh]


parseGitHttps :: Parser GitDetails
parseGitHttps = do
  string "https://github.com/"
  username <- takeTill (=='/')
  AT.take 1
  repo <- takeTill (=='.')
  return GitDetails{..}


parseGitSsh :: Parser GitDetails
parseGitSsh = do
  string "git@github.com:"
  username <- takeTill (=='/')
  AT.take 1
  repo <- takeTill (=='.')
  return GitDetails{..}


data GitDetails = GitDetails
  { username :: Text
  , repo     :: Text
  } deriving (Show, Eq)


data Config = Config
  { packages :: [Package]
  } deriving (Show, Eq)


data Package = Package
  { location :: Location
  } deriving (Show, Eq)


data Location = SimpleLocation Text
              | RemoteLocation GitLocation
              | NotSupportedLocation
  deriving (Show, Eq)


data GitLocation = GitLocation
  { git :: Maybe Text
  , commit :: Maybe Text
  } deriving (Show, Eq)


instance FromJSON Config where
  parseJSON (Y.Object v) = Config <$> v .: "packages"
  parseJSON _ = fail "Expected object for config value"


instance FromJSON Package where
  parseJSON (Y.Object v) = Package <$> v .: "location"
  parseJSON _ = fail "Expected object for config value"


instance FromJSON Location where
  parseJSON v =   (SimpleLocation <$> parseJSON v)
              <|> (RemoteLocation <$> parseJSON v)
              <|> pure NotSupportedLocation


instance FromJSON GitLocation where
  parseJSON (Y.Object v) = GitLocation <$> v .: "git"
                                       <*> v .: "commit"
  parseJSON _ = fail "Expected object for config value"
