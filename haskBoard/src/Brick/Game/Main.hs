module Brick.Game.Tui
    where

type Name = ()

data TUIMode options = ShowState | Ask options | EndGame

data TUIState = TUIState { gameStateView :: NMView
                         , viewer :: Player
                         , tuiMode :: TUIMode NMOptions
                         , eventQueue :: [NMEvent]
                         , brickToGameChan :: BChan NMPlayName
                         , winner :: Maybe Player
                         , batchUpdates :: Bool
                         } deriving (Generic)


makeFields 'TUIState

