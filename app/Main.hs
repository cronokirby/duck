module Main (main) where

import Relude
import System.Process (callProcess)

-- | A handle to a github repository.
--
-- Both the owner and the name of the repository are needed.
data Repo = Repo
  { owner :: Text,
    name :: Text
  }
  deriving (Show)

-- | Arguments to the program.
data Args = Args
  { -- | If present, use a specific repository.
    repo :: Maybe Repo
  }
  deriving (Show)

-- | Parse arguments.
--
-- This assumes that we've already removed the program name from the argument list.
parseArgs :: [Text] -> Args
parseArgs = \case
  owner : name : _ -> Args {repo = Just (Repo {owner, name})}
  _ -> Args {repo = Nothing}

-- | Run the program, given the arguments.
run :: Args -> IO ()
run args = do
  print args
  putTextLn "Calling GH"
  callProcess "gh" ["--version"]

main :: IO ()
main = getArgs >>= (map fromString >>> parseArgs >>> run)
