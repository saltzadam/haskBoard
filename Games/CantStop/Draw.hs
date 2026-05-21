module Draw
  ( theAttrMap,
    tempMarkAttr,
    printPlay,
  )
where

import Brick
import Brick.Game.Tui (defaultPlayerAttrs)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Objects (CantStopPlayName (..), TrackName)

emptyAttr :: AttrName
emptyAttr = attrName "empty"

tempMarkAttr :: AttrName
tempMarkAttr = attrName "tempMark"

theAttrMap :: AttrMap
theAttrMap =
  attrMap
    (V.brightRed `on` V.black)
    [ (defaultPlayerAttrs !! 0, V.cyan `on` V.black),
      (defaultPlayerAttrs !! 1, V.blue `on` V.black),
      (defaultPlayerAttrs !! 2, V.green `on` V.black),
      (defaultPlayerAttrs !! 3, V.yellow `on` V.black),
      (tempMarkAttr, V.white `on` V.black),
      (emptyAttr, V.rgbColor 160 160 160 `on` V.black)
    ]

writeTrack :: TrackName -> String
writeTrack track = show (2 + fromEnum track)

printPlay :: CantStopPlayName -> Text
printPlay (TwoMove _ track track') =
  if track == track'
    then T.pack $ "Move on " ++ writeTrack track ++ "(x2)"
    else T.pack $ "Move on " ++ writeTrack track ++ " and " ++ writeTrack track'
printPlay (Stop _) = T.pack "Stop"
printPlay (DontStop _) = T.pack "Don't stop"
printPlay (ForceStop _) = T.pack "owned"
printPlay (OneMove _ track) = T.pack $ "Move on " ++ writeTrack track
