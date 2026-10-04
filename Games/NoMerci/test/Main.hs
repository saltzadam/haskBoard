module Main where

import Control.Concurrent (forkIO)
import Control.Lens ((%~), (&), (^.))
import Control.Monad (forM_, void)
import Data.Finitary (inhabitants)
import qualified Data.Foldable as F
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import FinitaryMap (ftAt)
import Game.GameAction (GameAction (..))
import Game.GameE (applyAction)
import Game.Location (LocationShape (..), howManyF, inventory, transfer)
import Game.Options (Options (..))
import Game.Player (Player (..), PlayerNum (..))
import Game.Rules (runQuery)
import Game.View (project, viewGameStateAs')
import Interface.Agent (randomAgent, runAgentIO)
import Interface.Controller (PlayerInterface (..), buildInterface)
import Interface.Hint (applyHints)
import Helpers (hasAny)
import NoMerci (noMerci, takeOverValued)
import NumberedPiece (NumberedPiece (..))
import Objects
import Run (runGameSeparateChannelsNoLogs)
import ShuffleRNG (shuffleList)
import System.Random (mkStdGen)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "NoMerci" [fullGameTests, queryTests, stepTests])

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
  howManyF (gs ^. #objects . #locations . ftAt BoxTop) isCard @?= 9

fullGameTests :: TestTree
fullGameTests =
  testGroup
    "full game"
    [ testCase "random 3-player game ends consistently" $ do
        (gs, winners) <- playRandomGame
        checkEndState gs winners
    ]

p1 :: Player
p1 = Player PlayerOne

-- | Initial state with a list of transfers applied directly to the locations.
withTransfers :: [(NMLocation, NMLocation, NMResource)] -> NMGameState
withTransfers moves =
  let (gs, _, _) = noMerci 3
   in foldl (\s (from, to, r) -> s & #objects . #locations %~ transfer r from to) gs moves

-- | The card with face value v. Piece numbers start at 0 and face values start at 3.
card :: Int -> NMResource
card v = Card (NumberedPiece (fromIntegral (v - 3)))

queryTests :: TestTree
queryTests =
  testGroup
    "queries"
    [ testCase "score: 11 chips, no cards" $ do
        let (gs, gr, _) = noMerci 3
        runQuery ((gr ^. #score) p1) gs @?= 11,
      testCase "score: 11 chips minus run 3-4 (counts 3)" $ do
        let (_, gr, _) = noMerci 3
            gs = withTransfers [(CardDeck, PlayerStuff p1, card 3), (CardDeck, PlayerStuff p1, card 4)]
        runQuery ((gr ^. #score) p1) gs @?= 8,
      testCase "takeOverValued hint fires on the player's view" $ do
        let gs = withTransfers ((CardDeck, CenterOfTableCard, card 3) : replicate 3 (PlayerStuff p1, ChipPile, Chip))
            opts = Options (NES.fromList (Take NE.:| [Decline])) p1
        applyHints (viewGameStateAs' gs p1) [takeOverValued] opts @?= Just Take,
      testCase "hints cannot see hidden locations" $ do
        let gs = withTransfers []
            opts = Options (NES.fromList (Take NE.:| [Decline])) p1
            h :: NMHint
            h _ = do
              deckHasCard <- CardDeck `hasAny` [card 3]
              pure (if deckHasCard then Just Take else Nothing)
        applyHints (viewGameStateAs' gs p1) [h] opts @?= Nothing
        applyHints (project gs) [h] opts @?= Just Take
    ]

deckList :: NMGameState -> [NMResource]
deckList gs = case gs ^. #objects . #locations . ftAt CardDeck of
  Deck s -> F.toList s
  _ -> []

stepTests :: TestTree
stepTests =
  testGroup
    "applyAction"
    [ testCase "transfer moves one chip" $ do
        let (gs, _, _) = noMerci 3
            (gs', _) = applyAction (MkTransfer (PlayerStuff p1) ChipPile Chip) (gs, mkStdGen 0)
            chips l = howManyF (gs' ^. #objects . #locations . ftAt l) (== Chip)
        chips (PlayerStuff p1) @?= 10
        chips ChipPile @?= 1
    , testCase "shuffle keeps the cards and depends on the seed" $ do
        let (gs, _, _) = noMerci 3
            shuffled seed = fst (applyAction (Shuffle CardDeck) (gs, mkStdGen seed))
            locInv s = inventory (s ^. #objects . #locations . ftAt CardDeck)
        locInv (shuffled 1) @?= locInv gs
        assertBool "order changed" (deckList (shuffled 1) /= deckList gs)
        assertBool "seed matters" (deckList (shuffled 1) /= deckList (shuffled 2))
        assertBool "generator advances" (snd (applyAction (Shuffle CardDeck) (gs, mkStdGen 1)) /= mkStdGen 1)
    , testCase "shuffleList edge cases" $ do
        fst (shuffleList ([] :: [Int]) (mkStdGen 0)) @?= []
        fst (shuffleList [7 :: Int] (mkStdGen 0)) @?= [7]
    ]
