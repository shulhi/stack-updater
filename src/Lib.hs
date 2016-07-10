{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}


module Lib
    ( someFunc
    ) where


import            Text.RawString.QQ
import            Text.Regex (mkRegex, subRegex)

import            Data.Text (Text)
import qualified  Data.Text as T
import            Data.ByteString (ByteString)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as BSL
import qualified  Data.Yaml as Y
import            Data.Yaml ( FromJSON(..)
                            , (.:)
                            , (.:?) )
import            Data.Attoparsec.Text
import            Data.Attoparsec.Text as AT
import            Data.Maybe (catMaybes, fromJust)
import            Data.Monoid ((<>))
import            Data.Vector (Vector)
import            Data.Map.Lazy (Map)
import qualified  Data.Map.Lazy as M

import            Control.Applicative
import            Control.Monad (forM_)
import            Control.Monad.Trans (liftIO)
import            Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither, left, right)

import qualified  GitHub.Endpoints.Repos.Commits as GH


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
      let locations = catMaybes $ fmap (\p -> extractGitLocations p) packages
          regexes = mkRegex . T.unpack . commit <$> locations
          newContent = foldl (\content rgx -> subRegex rgx content "newcommit") contents regexes
          gitInfos = fmap (\loc -> GitInfo loc $ parseGitUrl . git $ loc) locations
      sequence_ $ getCommits <$> gitInfos
  where
    extractGitLocations p = case location p of
                              RemoteLocation remote -> Just remote
                              _ -> Nothing


getCommits :: GitInfo
           -> IO ()
getCommits info = do
  res <- runEitherT $ do
    details  <- hoistEither $ gitDetails info
    eCommits <- liftIO $ GH.commitsFor' (Just $ GH.BasicAuth user password) (mkUsername details) (mkRepo details)
    case eCommits of
      Left err -> left $ show err
      Right commits -> right commits
  case res of
    Left err -> print err
    Right vcs -> do
      print info
      forM_ vcs (\vc -> print $ GH.gitCommitMessage (GH.commitGitCommit vc))
  where
    user = "shulhi"
    password = ""
    mkUsername = GH.mkOwnerName . username
    mkRepo = GH.mkRepoName . repo


mkCommitMap :: GitDetails
            -> Vector GH.Commit
            -> Map Text (Vector GH.Commit)
mkCommitMap details commits = M.singleton repoId commits
  where
    repoId = (username details) <> "/" <> (repo details)


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
