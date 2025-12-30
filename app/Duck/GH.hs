module Duck.GH (Repo (..), Ctx (..), pullRequestFetch) where

import Control.Exception (throw)
import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Types (Parser, parseEither)
import Relude
import System.Process.Typed (proc, readProcessStdout_)

newtype ParserM a = ParserM (ReaderT Value Parser a)
  deriving (Functor, Applicative, Monad)

data ApiException = ApiParseException Text
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

data Ctx = Ctx
  { repo :: Maybe Repo
  }

ctxOwnerName :: Ctx -> (Text, Text)
ctxOwnerName ctx = case ctx.repo of
  Nothing -> (":owner", ":name")
  Just repo -> (repo.owner, repo.name)

type GqlQuery = Text

api :: GqlQuery -> (Value -> Parser Value) -> Ctx -> IO Value
api query p ctx = do
  out <- readProcessStdout_ (proc "gh" args)
  parse_ p out
  where
    (owner, name) = ctxOwnerName ctx
    args =
      [ "api",
        "graphql",
        "-f",
        "query=" <> toString query,
        "-f",
        "owner=" <> toString owner,
        "-f",
        "name=" <> toString name
      ]

pullRequestFetch :: Ctx -> IO ()
pullRequestFetch ctx = do
  v <- api query return ctx
  print v
  where
    query :: GqlQuery
    query =
      mconcat
        [ "query($owner: String!, $name: String!) {",
          "  repository(owner: $owner, name: $name) {",
          "     pullRequests(first: 10, states: OPEN) {",
          "     nodes {",
          "       title",
          "     } ",
          "     }",
          "  }",
          "}"
        ]
