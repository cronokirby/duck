module Main (main) where

import Duck.GH (Ctx (..), Repo (..), pullRequestFetch)
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
run args = do
  let ctx = Ctx {repo = args.repo}
  putTextLn "Calling GH"
  prs <- pullRequestFetch ctx
  print prs

main :: IO ()
main = getArgs >>= (map fromString >>> parseArgs >>> run)
