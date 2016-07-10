{-# LANGUAGE OverloadedStrings #-}

module Main where


import System.IO
import Control.Exception (bracket_)
import Options.Applicative


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
  print user
  print password
  case cmd of
    Update repoName -> updateRepo repoName
    Interactive -> print "Let's do it interactively"


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


prompt :: String
       -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine


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
           -> IO ()
updateRepo repo = do
  return ()
