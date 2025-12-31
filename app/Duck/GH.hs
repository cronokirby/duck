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

pullRequestFetch :: Ctx -> IO [Text]
pullRequestFetch =
  api
    $ Q.root [("$owner", "String!"), ("$name", "String!")]
    $ Q.object "repository" [("owner", "$owner"), ("name", "$name")]
    $ Q.object "pullRequests" [("first", "10"), ("states", "OPEN")]
    $ Q.list "nodes" []
    $ Q.field "title" Q.text
