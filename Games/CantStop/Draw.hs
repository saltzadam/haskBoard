module Draw
  ( player4Attr,
    player1Attr,
    player2Attr,
    player3Attr,
    theAttrMap,
    playerToColor,
    printOptions,
    tempMarkAttr,
  )
where

import Brick
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Game.Options (Options (..))
import Game.Player (Player (..))
import qualified Graphics.Vty as V
import Objects (CantStopOptions, CantStopPlayName (..), TrackName)

player1Attr, player2Attr, player3Attr, player4Attr :: AttrName
player1Attr = attrName "player0"
player2Attr = attrName "player1"
player3Attr = attrName "player2"
player4Attr = attrName "player3"

playerAttrs :: [AttrName]
playerAttrs = [player1Attr, player2Attr, player3Attr, player4Attr]

emptyAttr :: AttrName
emptyAttr = attrName "empty"

tempMarkAttr :: AttrName
tempMarkAttr = attrName "tempMark"

theAttrMap :: AttrMap
theAttrMap =
  attrMap
    (V.brightRed `on` V.black)
    [ (player1Attr, V.cyan `on` V.black),
      (player2Attr, V.blue `on` V.black),
      (player3Attr, V.green `on` V.black),
      (player4Attr, V.yellow `on` V.black),
      (tempMarkAttr, V.white `on` V.black),
      (emptyAttr, V.rgbColor 160 160 160 `on` V.black)
    ]

playerToColor :: Player -> AttrName
playerToColor (Player i) = playerAttrs !! (fromEnum i + 1)

writeTrack :: TrackName -> String
writeTrack track = show (2 + fromEnum track)

writePlayer :: Player -> String
writePlayer (Player i) = "Player " ++ show i

printPlay :: CantStopPlayName -> Text
printPlay (TwoMove _ track track') =
  if track == track'
    then T.pack $ "Move on " ++ writeTrack track ++ "(x2)"
    else T.pack $ "Move on " ++ writeTrack track ++ " and " ++ writeTrack track'
printPlay (Stop _) = T.pack "Stop"
printPlay (DontStop _) = T.pack "Don't stop"
printPlay (ForceStop _) = T.pack "owned"
printPlay (OneMove _ track) = T.pack $ "Move on " ++ writeTrack track

printOptions :: CantStopOptions -> Text
printOptions (Options legal' _) =
  let legal = NE.toList legal'
      printEnumeratedPlay :: (Int, CantStopPlayName) -> Text
      printEnumeratedPlay (i, play) = T.pack (show i ++ ") ") <> printPlay play
   in T.unlines . fmap printEnumeratedPlay $ zip [1 ..] legal
