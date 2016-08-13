{-# LANGUAGE OverloadedStrings #-}

module Types where



import            Data.Text                   (Text)
import qualified  Data.Yaml                   as Y
import            Data.Yaml                   ( Object(..)
                                              , FromJSON(..)
                                              , (.:)
                                              , (.:?) )
import            Control.Applicative


data GitInfo = GitInfo
  { gitLocation :: GitLocation
  , gitDetails :: Either String GitDetails
  } deriving (Show, Eq)


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
  { git :: Text
  , commit :: Text
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
