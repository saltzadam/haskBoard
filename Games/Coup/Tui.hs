{-# LANGUAGE OverloadedStrings #-}

module Tui (app) where

import Brick
import Brick.Game.Tui
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Table (ColumnAlignment (AlignLeft), columnBorders, renderTable, rowBorders, setDefaultColAlignment, surroundingBorder, table)
import Control.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Game.Player (Player, displayPlayer)
import qualified Graphics.Vty as V
import Helpers
import Objects
import Util (tshow)

type CoupTUIState = TUIState CoupLocation CoupCounter CoupResource CoupPhaseName CoupPlayName

type Name = ()

type Anns = [(Maybe Player, Text)]

app :: App CoupTUIState CoupEvent Name
app =
  App
    { appDraw = renderUIView,
      appChooseCursor = neverShowCursor,
      appHandleEvent = runHandler simpleHandler,
      appStartEvent = return (),
      appAttrMap = const theAttrMap
    }

renderUIView :: CoupTUIState -> [Widget Name]
renderUIView tui =
  let g = tui ^. #gameStateView
      anns = tui ^. #announcements
   in [ hLimit 100 $
          withAttr titleAttr (str "=== COUP ===")
            <=> renderActionTable anns g -- (C) tabular per-player last action
            <=> renderPlayers g -- Players block (coins / influence / lost)
            <=> renderMenu tui -- the action box
            <=> renderActionLog anns -- (A) chronological log, below the action box
      ]

-- (A) Chronological log ---------------------------------------------------------

-- | The most recent announcement is the "current action" and is highlighted; a
-- few prior lines follow for context.
renderActionLog :: Anns -> Widget Name
renderActionLog [] = emptyWidget
renderActionLog anns =
  borderWithLabel (str " What's happening ") . padLeftRight 1 $
    vBox (zipWith line [0 :: Int ..] (take 7 anns))
  where
    line 0 ann = withAttr currentActionAttr (txt ("> " <> printAnnouncement ann))
    line _ ann = withAttr pastActionAttr (txt ("  " <> printAnnouncement ann))

-- | An attributed announcement is prefixed with its speaker, as in
-- 'drawAnnouncements'.
printAnnouncement :: (Maybe Player, Text) -> Text
printAnnouncement (Nothing, msg) = msg
printAnnouncement (Just p, msg) = T.pack (displayPlayer p) <> ": " <> msg

-- (C) Tabular per-player last action -------------------------------------------

-- | A compact table: one row per player showing their most recent action,
-- separate from the Players block.
renderActionTable :: Anns -> CoupView -> Widget Name
renderActionTable anns g =
  let current = viewCurrentPlayer g
      marker p =
        padRight (Pad 1) $
          if p == current then withAttr currentActionAttr (str "▶") else str " "
   in borderWithLabel (str " Recent actions ") . padLeftRight 1 $
        renderTable $
          setDefaultColAlignment AlignLeft $
            surroundingBorder False $
              rowBorders False $
                columnBorders False $
                  table
                    [ [ marker p,
                        padRight (Pad 2) (withAttr titleAttr (str (displayPlayer p))),
                        hLimit 70 (txtWrap (lastActionOf anns p))
                      ]
                    | p <- S.toList (g ^. #playersView)
                    ]

-- | The most recent announcement tagged with a given player. "—" if none yet.
lastActionOf :: Anns -> Player -> Text
lastActionOf anns p =
  case [msg | (Just q, msg) <- anns, q == p] of
    (msg : _) -> msg
    [] -> "—"

-- (B) Players block -------------------------------------------------------------

renderPlayers :: CoupView -> Widget Name
renderPlayers g =
  borderWithLabel (str " Players ") . padLeftRight 1 $
    vBox (renderPlayer g <$> S.toList (g ^. #playersView))

renderPlayer :: CoupView -> Player -> Widget Name
renderPlayer g p =
  padTop (Pad 1) $
    str (displayPlayer p)
      <=> str ("  Coins:     " ++ show (coinsOf g p))
      <=> str ("  Influence: " ++ printInfluence g p)
      <=> str ("  Lost:      " ++ printRevealed g p)

-- Menu -------------------------------------------------------------------------

renderMenu :: CoupTUIState -> Widget Name
renderMenu tui =
  padTop (Pad 1) . hLimit 60 $
    case tui ^. #tuiMode of
      EndGame -> renderEndGame tui
      Ask o ->
        let p = viewCurrentPlayer (tui ^. #gameStateView)
         in borderWithLabel (str " Your decision ") . padLeftRight 1 $
              withAttr promptAttr (str (promptLabel o))
                <=> withAttr pastActionAttr (str ("(during " ++ displayPlayer p ++ "'s turn)"))
                <=> hBorder
                <=> drawOptions printPlay o
      ShowState ->
        let p = viewCurrentPlayer (tui ^. #gameStateView)
         in str (displayPlayer p ++ "'s turn — waiting for other players...")

-- | What the current prompt is actually asking, inferred from the legal plays,
-- so an out-of-turn reaction reads clearly (not as "it's someone's turn").
promptLabel :: CoupOptions -> String
promptLabel o =
  let plays = foldr (:) [] (o ^. #legal)
      isReveal (Reveal _) = True
      isReveal _ = False
      isReturn (ReturnCard _) = True
      isReturn _ = False
      isBlock x = x `elem` [BlockForeignAid, BlockStealCaptain, BlockStealAmbassador, BlockAssassination]
   in if any isReveal plays
        then "You are losing influence — choose a card to reveal:"
        else if any isReturn plays
        then "Exchange — choose a card to return to the court deck:"
        else if Challenge `elem` plays
          then "React: challenge the claim, or allow it?"
          else if any isBlock plays
            then "React: block this action, or allow it?"
            else "Your turn — choose an action:"

renderEndGame :: CoupTUIState -> Widget Name
renderEndGame tui =
  borderWithLabel (str " Game over ") . padLeftRight 1 $
    drawEndGame (tui ^. #winner)
      <=> str " "
      <=> withAttr promptAttr (str "[Enter] play again    [q] quit")

printPlay :: CoupPlayName -> Text
printPlay Income = "Income (+1 coin)"
printPlay ForeignAid = "Foreign Aid (+2 coins)"
printPlay TakeTax = "Tax (claim Duke, +3)"
printPlay (LaunchCoup p) = "Coup " <> T.pack (displayPlayer p) <> " (pay 7)"
printPlay (Assassinate p) = "Assassinate " <> T.pack (displayPlayer p) <> " (claim Assassin, pay 3)"
printPlay (Steal p) = "Steal from " <> T.pack (displayPlayer p) <> " (claim Captain)"
printPlay ExchangeCards = "Exchange (claim Ambassador)"
printPlay Challenge = "Challenge the claim!"
printPlay AllowIt = "Allow it"
printPlay BlockForeignAid = "Block it (claim Duke)"
printPlay BlockStealCaptain = "Block with Captain"
printPlay BlockStealAmbassador = "Block with Ambassador"
printPlay BlockAssassination = "Block it (claim Contessa)"
printPlay (Reveal r) = "Give up your " <> tshow r
printPlay (ReturnCard r) = "Return a " <> tshow r <> " to the deck"

-- Player state readouts ---------------------------------------------------------

coinsOf :: CoupView -> Player -> Int
coinsOf g p = fromMaybe 0 (viewCounterVal g (PlayerCoins p))

-- | Roles at a location ([] if hidden).
rolesAt :: CoupView -> CoupLocation -> [Role]
rolesAt g l = maybe [] (map roleOf . S.toList) (viewWhatsAt g l)

-- | Your own hand is visible; opponents' face-down cards are not, so we show
-- the count deduced from public info (started with 2, minus what they've lost).
printInfluence :: CoupView -> Player -> String
printInfluence g p =
  case viewLocation g (Influence p) of
    Just _ ->
      let roles = rolesAt g (Influence p)
       in if null roles then "(out)" else unwords (map show roles)
    Nothing ->
      let remaining = 2 - length (rolesAt g (Revealed p))
       in if remaining <= 0 then "(out)" else show remaining ++ " face-down"

printRevealed :: CoupView -> Player -> String
printRevealed g p =
  let rs = rolesAt g (Revealed p)
   in if null rs then "(none)" else unwords (map show rs)

-- Attributes -------------------------------------------------------------------

titleAttr, currentActionAttr, pastActionAttr, promptAttr :: AttrName
titleAttr = attrName "title"
currentActionAttr = attrName "currentAction"
pastActionAttr = attrName "pastAction"
promptAttr = attrName "prompt"

theAttrMap :: AttrMap
theAttrMap =
  attrMap
    (V.white `on` V.black)
    [ (titleAttr, V.withStyle (fg V.cyan) V.bold),
      (currentActionAttr, V.withStyle (fg V.yellow) V.bold),
      (pastActionAttr, fg V.brightBlack),
      (promptAttr, fg V.green)
    ]
