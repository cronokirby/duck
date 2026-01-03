module Duck.Tui (runTui) where

import Brick qualified as B
import Brick.Keybindings qualified as B
import Brick.Main (App (..))
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as B
import Duck.GH (Repo)
import Graphics.Vty qualified as B
import Relude

-- | One of the possible events in our application.
data Event = NextTab | PreviousTab | Halt deriving (Eq, Ord, Bounded, Enum, Show)

keyConfig :: B.KeyConfig Event
keyConfig =
  B.newKeyConfig
    ( B.keyEvents
        [ ("next tab", NextTab),
          ("previous tab", PreviousTab),
          ("halt", Halt)
        ]
    )
    [ (NextTab, [B.bind '\t']),
      (PreviousTab, [B.bind B.KBackTab]),
      (Halt, [B.bind 'q', B.bind B.KEsc])
    ]
    []

keyDispatcher :: B.KeyDispatcher Event M
keyDispatcher =
  either (\_ -> error "invalid key config") id
    $ B.keyDispatcher keyConfig ((\e -> B.onEvent e (show e) . void $ handleEvent e) <$> universe)

-- | A possible tab, for the top level menu.
data Tab = PrListTab | PrTab | OpenFileTab deriving (Eq, Bounded, Enum)

satSucc :: (Eq a, Bounded a, Enum a) => a -> a
satSucc a = if a == maxBound then a else succ a

satPred :: (Eq a, Bounded a, Enum a) => a -> a
satPred a = if a == minBound then a else pred a

data AppState = AppState
  { which :: Tab
  }

initialState :: AppState
initialState = AppState {which = PrListTab}

type M = B.EventM () AppState

handleEvent :: Event -> M ()
handleEvent = \case
  NextTab -> modify (\s -> s {which = satSucc s.which})
  PreviousTab -> modify (\s -> s {which = satPred s.which})
  Halt -> B.halt

data StyleRole = StyleFocused deriving (Show)

-- | The global attribute map
attrMap :: B.AttrMap
attrMap = B.attrMap B.defAttr (styles & map (\(n, a) -> (B.attrName (show n), a)))
  where
    styles :: [(StyleRole, B.Attr)]
    styles =
      [ ( StyleFocused,
          B.defAttr & (`B.withForeColor` B.black) & (`B.withBackColor` B.white)
        )
      ]

withStyle :: StyleRole -> B.Widget n -> B.Widget n
withStyle r = B.withAttr (B.attrName (show r))

drawTab :: Bool -> Text -> B.Widget n
drawTab focused t = inner
  where
    modifier =
      if focused
        then withStyle StyleFocused
        else id
    inner = modifier $ B.padLeftRight 1 (B.txt t)

draw :: AppState -> B.Widget n
draw s =
  B.withBorderStyle B.unicode . B.joinBorders . B.borderWithLabel (B.txt "Duck") . B.vLimit 1 $ tabs
  where
    tabs =
      B.hBox
        [ oneTab ("PRs", PrListTab),
          B.vBorder,
          oneTab ("#1023", PrTab),
          B.vBorder,
          oneTab ("File", OpenFileTab),
          B.vBorder,
          B.fill ' '
        ]
    oneTab (name, tab) = drawTab (s.which == tab) name

app :: App AppState Event ()
app =
  App
    { appDraw = pure <$> draw,
      appChooseCursor = \_ _ -> Nothing,
      appHandleEvent = \case
        B.VtyEvent (B.EvKey k ms) -> do
          match <- B.handleKey keyDispatcher k ms
          if match
            then pure ()
            else B.continueWithoutRedraw
        _ -> B.continueWithoutRedraw,
      appStartEvent = pure (),
      appAttrMap = \_ -> attrMap
    }

runTui :: Maybe Repo -> IO ()
runTui _ = void (B.defaultMain app initialState)
