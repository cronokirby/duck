module Duck.GH (PullRequest (..)) where

import Relude

data PullRequest = PullRequest
  { title :: Text
  }
  deriving (Generic, Show)

pullRequestFetch :: IO [PullRequest]
pullRequestFetch = undefined
