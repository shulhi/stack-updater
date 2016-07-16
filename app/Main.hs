{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import System.IO
import Control.Exception (bracket_)
import Control.Monad (void, join, sequence_)
import Options.Applicative

import Data.Vector (Vector, imapM_, (!?))
import qualified  Data.Vector as V
import qualified  Data.Text as T
import qualified  Data.Map.Lazy as M
import Text.Read (readMaybe)

import qualified  GitHub.Endpoints.Repos.Commits as GH


type RepoName = String

data Command
  = Update (Maybe RepoName)
  | Rollback RepoName
  | Interactive


data Options = Options Command


main :: IO ()
main = run =<< execParser
    (parseOptions `withInfo` "Update git-based location in stack.yaml with latest commit from Github")


parseOptions :: Parser Options
parseOptions = Options <$> parseCommand


parseCommand :: Parser Command
parseCommand = subparser $
  command "update" (parseUpdate `withInfo` "Update specified repo to latest commit")


parseUpdate :: Parser Command
parseUpdate = Update <$> (optional $ argument str (metavar "REPO-NAME"))


-- Actual program logic
run :: Options -> IO ()
run (Options cmd) = do
  user <- prompt "Username: "
  password <- getPassword
  case cmd of
    Update Nothing -> displayGitLocations (user, password)
    Update (Just repoName) -> updateRepo repoName (user, password)
    _ -> print "Not supported yet"


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


prompt :: String
       -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine


confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? y/N"
  ans <- prompt ">> "
  if ans == "y"
     then return True
     else return False


-- http://stackoverflow.com/questions/4064378/prompting-for-a-password-in-haskell-command-line-application
getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass


withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action


updateRepo :: String
           -> (String, String)
           -> IO ()
updateRepo repo credential = do
  gitLocations <- getGitLocations
  case M.lookup (T.pack repo) gitLocations of
    Nothing -> putStrLn $ "Repo " <> repo <> " is not found in stack.yaml"
    Just info -> do
      commits <- getCommits info credential
      displaySelectCommit commits info


updateRepo' :: GitInfo
            -> (String, String)
            -> IO ()
updateRepo' info credential = do
  commits <- getCommits info credential
  displaySelectCommit commits info


displayGitLocations :: (String, String)
                    -> IO ()
displayGitLocations credential = do
  gitLocations <- getGitLocations
  putStrLn "Github repos: "
  formatGitLocationDisplay $ M.keys gitLocations
  selection <- prompt ">> "
  res <- confirm
  case res of
    False -> displayGitLocations credential
    True -> do
      let mSelected = getSelection selection $ V.fromList $ M.keys gitLocations
          mInfo = join $ flip M.lookup gitLocations <$> mSelected
      sequence_ $ flip updateRepo' credential <$> mInfo


formatGitLocationDisplay :: [T.Text]
                         -> IO ()
formatGitLocationDisplay locations = imapM_ displayLocation $ V.fromList locations
  where
    displayLocation idx loc = do
      putStrLn $ (show idx) <> " - " <> (T.unpack loc)


displaySelectCommit :: Vector GH.Commit
                    -> GitInfo
                    -> IO ()
displaySelectCommit commits info = do
  putStrLn "Latest 10 commits. Please select commit: "
  formatCommitDisplay commits (Just 10)
  selection <- prompt ">> "
  res <- confirm
  case res of
    False -> displaySelectCommit commits info
    True -> do
      let oldCommit = T.unpack . commit . gitLocation $ info
          mNewCommit = T.unpack . GH.untagName . GH.commitSha <$> getSelection selection commits
      case mNewCommit of
        Nothing -> putStrLn "Selection invalid or commit not found"
        Just newCommit -> replaceCommit oldCommit newCommit


formatCommitDisplay :: Vector GH.Commit
                    -> Maybe Int
                    -> IO ()
formatCommitDisplay commits mLimit = imapM_ displayInfo commits'
  where
    limit = case mLimit of
              Nothing -> 10
              Just x -> x
    commits' = V.take limit commits
    displayInfo idx commit = do
      putStrLn $ (show idx) <> " - " <> (T.unpack . commitSha $ commit) <> " " <> (T.unpack . commitMsg $ commit)
    commitSha = GH.untagName . GH.commitSha
    commitMsg commit = GH.gitCommitMessage $ GH.commitGitCommit commit


getSelection :: String
             -> Vector a
             -> Maybe a
getSelection selectionIdx choices = case readMaybe selectionIdx of
                                      Nothing -> Nothing
                                      Just idx -> choices !? idx
