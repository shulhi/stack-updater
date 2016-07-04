{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module Lib
    ( someFunc
    ) where


import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Yaml as Y
import Data.Yaml
    ( FromJSON(..)
    , (.:)
    , (.:?) )
import Text.RawString.QQ
import Control.Applicative

someFunc :: IO ()
someFunc = print $ (Y.decodeEither sampleYaml :: Either String Config)

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
|]


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
  { git :: Maybe String
  , commit :: Maybe String
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
