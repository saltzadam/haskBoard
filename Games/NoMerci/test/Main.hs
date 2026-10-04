module Main where

import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad (forM_, void)
import Data.Finitary (inhabitants)
import Data.Generics.Labels ()
import qualified Data.Map as M
import qualified Data.Set as S
import FinitaryMap (ftAt)
import Game.Location (howMany', howManyF)
import Game.Player (Player)
import Interface.Agent (randomAgent, runAgentIO)
import Interface.Controller (PlayerInterface (..), buildInterface)
import NoMerci (noMerci)
import Objects
import Run (runGameSeparateChannelsNoLogs)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "NoMerci" [fullGameTests])

-- | Play a full 3-player game with random agents through the channel API.
playRandomGame :: IO (NMGameState, [Player])
playRandomGame = do
  let (gs, gr, _) = noMerci 3
  controller <- buildInterface (S.toList (gs ^. #players))
  forM_ (M.elems (controller ^. #playerInterfaces)) $ \(PlayerInterface from to) ->
    void (forkIO (runAgentIO (randomAgent [] from to)))
  runGameSeparateChannelsNoLogs controller gs gr

totalOf :: (NMResource -> Bool) -> NMGameState -> Int
totalOf p gs = sum [howManyF (gs ^. #objects . #locations . ftAt l) p | l <- inhabitants @NMLocation]

checkEndState :: NMGameState -> [Player] -> Assertion
checkEndState gs winners = do
  assertBool "someone wins" (not (null winners))
  totalOf (== Chip) gs @?= 33 -- 3 players start with 11 chips each
  totalOf isCard gs @?= 33 -- all cards still exist
  howManyF (gs ^. #objects . #locations . ftAt CardDeck) isCard @?= 0 -- game ends on empty deck
  howMany' (gs ^. #objects . #locations . ftAt BoxTop) Chip @?= 0

fullGameTests :: TestTree
fullGameTests =
  testGroup
    "full game"
    [ testCase "random 3-player game ends consistently" $ do
        (gs, winners) <- playRandomGame
        checkEndState gs winners
    ]
