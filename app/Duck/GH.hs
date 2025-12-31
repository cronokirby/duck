{-# LANGUAGE ApplicativeDo #-}

module Duck.GH (Repo (..), Ctx (..), pullRequestFetch) where

import Control.Exception (throw)
import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Types (Parser, parseEither)
import Duck.GraphQl qualified as Q
import Relude
import System.Process.Typed (proc, readProcessStdout_)

newtype ParserM a = ParserM (ReaderT Value Parser a)
  deriving (Functor, Applicative, Monad)

newtype ApiException = ApiParseException Text
  deriving (Show)

-- | Parse a value from a lazy bytestring, throwing an exception on failure.
parse_ :: (Value -> Parser a) -> LByteString -> IO a
parse_ p =
  (eitherDecode @Value >=> parseEither p) >>> either (toText >>> ApiParseException >>> throw) return

instance Exception ApiException

data Repo = Repo
  { owner :: Text,
    name :: Text
  }
  deriving (Show)

newtype Ctx = Ctx
  { repo :: Maybe Repo
  }

ctxOwnerName :: Ctx -> (Text, Text)
ctxOwnerName ctx = case ctx.repo of
  Nothing -> (":owner", ":name")
  Just repo -> (repo.owner, repo.name)

api :: Q.Query a -> Ctx -> IO a
api query ctx = do
  out <- readProcessStdout_ (proc "gh" args)
  parse_ (Q.parse query) out
  where
    (owner, name) = ctxOwnerName ctx
    args =
      [ "api",
        "graphql",
        "-f",
        "query=" <> toString (Q.build query),
        "-f",
        "owner=" <> toString owner,
        "-f",
        "name=" <> toString name
      ]

-- | An overview of a pull request.
--
-- Specifically, information like the title, url, etc. but not the contents
-- itself.
data PullRequest = PullRequest
  { -- | The pull request number.
    id :: Int,
    -- | The title of the pull request.
    title :: Text,
    -- | A link to this pull request.
    url :: Text
  }
  deriving (Show)

-- | Fetch the list of pull requests, given a context.
pullRequestFetch :: Ctx -> IO [PullRequest]
pullRequestFetch =
  api
    $ Q.root [("$owner", "String!"), ("$name", "String!")]
    $ Q.object "repository" [("owner", "$owner"), ("name", "$name")]
    $ Q.object "pullRequests" [("first", "10"), ("states", "OPEN")]
    $ Q.object "nodes" []
    $ Q.list
    $ do
      id' <- Q.field "number" Q.int
      title <- Q.field "title" Q.text
      url <- Q.field "url" Q.text
      pure PullRequest {id = id', title, url}
