{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import System.IO
import Control.Exception (bracket_)
import Options.Applicative

import Data.Vector
import qualified  Data.Vector as V
import qualified  Data.Text as T
import qualified  Data.Map.Lazy as M
import Text.Read (readMaybe)

import qualified  GitHub.Endpoints.Repos.Commits as GH


type RepoName = String

data Command
  = Update RepoName
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
  command "update" (parseUpdate `withInfo` "Update specified repo to latest commit") <>
  command "interactive" (parseInteractive `withInfo` "Do it interactively")


parseUpdate :: Parser Command
parseUpdate = Update
  <$> argument str (metavar "REPO-NAME")


parseInteractive :: Parser Command
parseInteractive = pure Interactive


-- Actual program logic
run :: Options -> IO ()
run (Options cmd) = do
  user <- prompt "Username: "
  password <- getPassword
  case cmd of
    Update repoName -> updateRepo repoName (user, password)
    Interactive -> print "Let's do it interactively"


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
      formatCommitDisplay commits (Just 10)
      putStrLn "Select commit: "
      selection <- prompt ">> "
      res <- confirm
      case res of
        False -> putStrLn "Aborting..."
        True -> do
          let oldCommit = T.unpack . commit . gitLocation $ info
              mNewCommit = T.unpack . GH.untagName . GH.commitSha <$> getSelection selection commits
          case mNewCommit of
            Nothing -> putStrLn "Commit not found"
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
      putStrLn $ (show idx) <> " - " <> (show . commitSha $ commit) <> " " <> (show . commitMsg $ commit)
    commitSha = GH.commitSha
    commitMsg commit = GH.gitCommitMessage $ GH.commitGitCommit commit


getSelection :: String
             -> Vector GH.Commit
             -> Maybe GH.Commit
getSelection selectionIdx commits = case readMaybe selectionIdx of
                                      Nothing -> Nothing
                                      Just idx -> commits !? idx
