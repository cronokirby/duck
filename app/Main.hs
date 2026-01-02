{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Duck.GH (Repo (..))
import Duck.Tui (runTui)
import Relude

-- | Arguments to the program.
newtype Args = Args
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
run args = runTui args.repo

main :: IO ()
main = getArgs >>= (map fromString >>> parseArgs >>> run)
