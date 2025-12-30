-- | Contains utilities for creating GraphQl queries.
module Duck.GraphQl (Query, build, parse, text, field, object, list, root) where

import Data.Aeson (Object, Value (Object), withArray, withObject, (.:), (<?>))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (JSONPathElement (Key), Parser, withText)
import Data.Foldable1 (foldlMap1')
import Data.Text.Lazy.Builder qualified as B
import Relude

data Query a where
  QText :: Query Text
  QField :: Text -> Query a -> Query a
  QList :: Text -> [(Text, Text)] -> Query a -> Query [a]
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
      QList name args q ->
        B.fromText name <> goArgs args <> "{\n" <> go q <> "\n}"
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
parse = goV ""
  where
    goV :: String -> Query a -> Value -> Parser a
    goV ctx q v = case q of
      QText -> withText ctx return v
      QField _ _ -> error "QField should be inside QObject or QList"
      QObject _ _ _ -> error "QObject should be inside QRoot"
      QList _ _ _ -> error "QList should be inside QRoot"
      QRoot _ qa -> (\f -> withObject "root" f v) $ \o -> do
        v' <- o .: "data" <?> Key "data"
        withObject "data" (goO "data" qa) v'
      QPure a -> pure a
      QAp qf qa -> goV ctx qf v <*> goV ctx qa v
    goO :: String -> Query a -> Object -> Parser a
    goO ctx q o = case q of
      QField name q' ->
        let name' = fromText name
         in (o .: name' <?> Key name') >>= goV ctx q'
      QObject name _ qa -> do
        let nameK = fromText name
        let nameS = toString name
        v' <- o .: nameK <?> Key nameK
        withObject nameS (goO nameS qa) v'
      QList name _ qa -> do
        let nameK = fromText name
        let nameS = toString name
        v' <- o .: nameK <?> Key nameK
        withArray nameS (traverse (withObject nameS (goO nameS qa)) >>> fmap toList) v'
      q' -> goV ctx q' (Object o)

text :: Query Text
text = QText

field :: Text -> Query a -> Query a
field = QField

list :: Text -> [(Text, Text)] -> Query a -> Query [a]
list = QList

object :: Text -> [(Text, Text)] -> Query a -> Query a
object = QObject

root :: [(Text, Text)] -> Query a -> Query a
root = QRoot
