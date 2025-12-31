-- | Contains utilities for creating GraphQl queries.
module Duck.GraphQl (Query, build, parse, text, field, object, list, root) where

import Data.Aeson (Value, withArray, withObject, (.:), (<?>))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (JSONPathElement (Key), Parser, withText)
import Data.Foldable1 (foldlMap1')
import Data.Text.Lazy.Builder qualified as B
import Relude

data Query a where
  QText :: Query Text
  QField :: Text -> Query a -> Query a
  QList :: Query a -> Query [a]
  QObject :: Text -> [(Text, Text)] -> Query a -> Query a
  QRoot :: [(Text, Text)] -> Query a -> Query a
  QPure :: a -> Query a
  QAp :: Query (a -> b) -> Query a -> Query b

instance Functor Query where
  fmap :: (a -> b) -> Query a -> Query b
  fmap f = QAp (QPure f)

instance Applicative Query where
  pure = QPure
  (<*>) = QAp

build :: Query a -> Text
build = go >>> B.toLazyText >>> toStrict
  where
    go :: Query a -> B.Builder
    go = \case
      QText -> mempty
      QField name _ -> B.fromText name
      QObject name args q ->
        B.fromText name <> goArgs args <> "{\n" <> go q <> "\n}"
      QList q -> go q
      QRoot args q ->
        "query" <> goArgs args <> "{\n" <> go q <> "\n}"
      QPure _ -> mempty
      QAp qf qa -> go qf <> "\n" <> go qa

    goArgs :: [(Text, Text)] -> B.Builder
    goArgs =
      viaNonEmpty (foldlMap1' m j)
        >>> maybe mempty (\x -> "(" <> x <> ")")
      where
        m (a, b) = B.fromText a <> ": " <> B.fromText b
        j acc x = acc <> ", " <> m x

parse :: Query a -> Value -> Parser a
parse = go ""
  where
    go :: String -> Query a -> Value -> Parser a
    go ctx q v = case q of
      QText -> withText ctx return v
      QField name q' -> (\f -> withObject ctx f v) $ \o -> do
        let name' = fromText name
        v' <- o .: name' <?> Key name'
        go (toString name) q' v'
      QObject name _ q' -> (\f -> withObject ctx f v) $ \o -> do
        let name' = fromText name
        v' <- o .: name' <?> Key name'
        go (toString name) q' v'
      QRoot _ q' -> (\f -> withObject "root" f v) $ \o -> do
        v' <- o .: "data" <?> Key "data"
        go "data" q' v'
      QList q' -> withArray ctx (traverse (go ctx q') >>> fmap toList) v
      QPure a -> pure a
      QAp qf qa -> go ctx qf v <*> go ctx qa v

text :: Query Text
text = QText

field :: Text -> Query a -> Query a
field = QField

list :: Query a -> Query [a]
list = QList

object :: Text -> [(Text, Text)] -> Query a -> Query a
object = QObject

root :: [(Text, Text)] -> Query a -> Query a
root = QRoot
