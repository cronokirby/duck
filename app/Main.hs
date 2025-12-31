{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Brick ((<+>))
import Brick qualified as B
import Brick.Widgets.List qualified as B
import Duck.GH (Ctx (..), PullRequest (..), Repo (..), pullRequestFetch)
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
  prs <- fromList @(Seq _) <$> pullRequestFetch (Ctx args.repo)
  B.simpleMain @Text $ B.renderList renderPr True (B.list "prs" prs 1)
  where
    renderPr :: Bool -> PullRequest -> B.Widget n
    renderPr _ pr =
      B.txt (show pr.id)
        <+> B.padLeft (B.Pad 1) (B.txt (show pr.title))

main :: IO ()
main = getArgs >>= (map fromString >>> parseArgs >>> run)
