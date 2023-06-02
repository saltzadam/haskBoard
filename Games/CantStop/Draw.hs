module Draw
    (player0Attr
    , player1Attr
    , player2Attr
    , player3Attr
    , theAttrMap
    , playerToColor
    , printOptions
    , tempMarkAttr
    )
where
import Brick
import qualified Graphics.Vty as V
import Game.Player (Player (..))
import Objects (CantStopOptions, CantStopPlayName (..))
import Data.Text (Text)
import Game.Options (Options(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

player0Attr, player1Attr, player2Attr, player3Attr :: AttrName
player0Attr = attrName "player0"
player1Attr = attrName "player1"
player2Attr = attrName "player2"
player3Attr = attrName "player3"

playerAttrs :: [AttrName]
playerAttrs = [player0Attr, player1Attr, player2Attr, player3Attr]

emptyAttr :: AttrName
emptyAttr = attrName "empty"

tempMarkAttr :: AttrName
tempMarkAttr = attrName "tempMark"

theAttrMap :: AttrMap
theAttrMap = attrMap (V.brightRed `on` V.black)
    [ (player0Attr, V.cyan `on` V.black),
      (player1Attr, V.blue `on` V.black),
      (player2Attr, V.green `on` V.black),
      (player3Attr, V.yellow `on` V.black),
      (tempMarkAttr, V.white `on` V.black),
      (emptyAttr, V.rgbColor 160 160 160  `on` V.black)
    ]

playerToColor :: Player -> AttrName
playerToColor (Player i) = playerAttrs !! fromIntegral i


printPlay :: CantStopPlayName -> Text
printPlay (TwoMove _ track track') = if track == track'
                                  then T.pack $ "Move on " ++ show track ++ " (2)"
                                  else T.pack $ "Move on " ++ show track ++ " and " ++ show track'
printPlay (Stop _) = T.pack "Stop"
printPlay (DontStop _) = T.pack "Don't stop"
printPlay (ForceStop _) = T.pack "owned"
printPlay (OneMove _ track) = T.pack $ "Move on " ++ show track

printOptions :: CantStopOptions -> Text
printOptions (Options legal' _ _) = let
    legal = NE.toList legal'
    printEnumeratedPlay :: (Int, CantStopPlayName) -> Text
    printEnumeratedPlay (i, play) = T.pack (show i ++ ") ") <> printPlay play
    in
    T.unlines . fmap printEnumeratedPlay $ zip [1..] legal
