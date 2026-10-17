{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, decode, encode)
import Data.Finitary (Finitary (..), inhabitants)
import Data.Finite (finite)
import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import FinitaryMap (FTMap (..), (!!!))
import GHC.Generics (Generic)
import Game.GameStateBase (GameState (..))
import Game.Location (GameObjects (..), LocationShape (..), NoCounters, dummyCounter)
import Game.Player (Player (..), PlayerNum (..), Turn (..), mkPlayers)
import Game.Rules (Query, runQuery)
import Game.Visibility (allVisible)
import Grid
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Helpers" [cellTests, geometryTests, queryTests])

-- Fixture: a 3-row, 4-column board.

type C = Cell 3 4

c :: Integer -> Integer -> C
c r col = Cell (finite r) (finite col)

data TLoc = Sq C | Bag
  deriving (Eq, Ord, Show, Generic, Finitary, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data TRes = X | O
  deriving (Eq, Ord, Show, Generic, Finitary, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

board :: Grid C TLoc
board = Grid (FTMap Sq)

-- | X at (0,0) and (2,3), O at (1,1); all other squares empty; Bag empty.
initLocs :: TLoc -> LocationShape TRes
initLocs (Sq x)
  | x == c 0 0 || x == c 2 3 = Slot (Just X)
  | x == c 1 1 = Slot (Just O)
  | otherwise = Slot Nothing
initLocs Bag = Pile mempty

testState :: GameState TLoc NoCounters TRes () ()
testState =
  let p1 = Player PlayerOne
      turn = Turn p1 (() NE.:| [])
   in GameState
        { players = S.fromList (mkPlayers 2),
          objects = GameObjects (FTMap initLocs) (FTMap (const dummyCounter)),
          currentPhase = (),
          currentTurn = turn,
          nextTurn = turn,
          visibility = allVisible
        }

runQ :: Query TLoc NoCounters TRes () () a -> a
runQ q = runQuery q testState

cellTests :: TestTree
cellTests =
  testGroup
    "Cell"
    [ testCase "has rows * cols inhabitants" $
        length (inhabitants @C) @?= 12,
      testCase "inhabitants are strictly ascending (reifyFn relies on this)" $
        let xs = inhabitants @C in and (zipWith (<) xs (drop 1 xs)) @? "not ascending",
      testCase "fromFinite . toFinite = id" $
        let xs = inhabitants @C in map (fromFinite . toFinite) xs @?= xs,
      testCase "JSON round trip" $
        decode (encode (c 2 3)) @?= Just (c 2 3),
      testCase "JSON rejects out-of-bounds cells" $
        decode (encode ((3, 0) :: (Integer, Integer))) @?= (Nothing :: Maybe C)
    ]

geometryTests :: TestTree
geometryTests =
  testGroup
    "geometry"
    [ testCase "offset inside the board" $
        offset (1, 2) (c 0 0) @?= Just (c 1 2),
      testCase "offset off the top" $
        offset (-1, 0) (c 0 0) @?= Nothing,
      testCase "offset off the right" $
        offset (0, 4) (c 0 0) @?= Nothing,
      testCase "neighbors4 in the interior" $
        sort (neighbors4 (c 1 1)) @?= [c 0 1, c 1 0, c 1 2, c 2 1],
      testCase "neighbors4 in a corner" $
        sort (neighbors4 (c 0 0)) @?= [c 0 1, c 1 0],
      testCase "neighbors8 in the interior" $
        length (neighbors8 (c 1 1)) @?= 8,
      testCase "neighbors8 in a corner" $
        sort (neighbors8 (c 0 0)) @?= [c 0 1, c 1 0, c 1 1],
      testCase "Coord neighbors for Cell is 4-adjacency" $
        sort (neighbors (c 1 1)) @?= sort (neighbors4 (c 1 1))
    ]

queryTests :: TestTree
queryTests =
  testGroup
    "queries"
    [ testCase "lookCell" $
        runQ (lookCell board (c 0 1)) @?= Slot Nothing,
      testCase "cellHas: yes" $
        runQ (cellHas board (c 1 1) O) @?= True,
      testCase "cellHas: wrong piece" $
        runQ (cellHas board (c 1 1) X) @?= False,
      testCase "occupied: yes" $
        runQ (occupied board (c 1 1)) @?= True,
      testCase "occupied: no" $
        runQ (occupied board (c 0 1)) @?= False,
      testCase "findPiece returns cells in row-major order" $
        runQ (findPiece board X) @?= [c 0 0, c 2 3],
      testCase "findPieces concatenates per piece" $
        runQ (findPieces board [O, X]) @?= [c 1 1, c 0 0, c 2 3],
      testCase "snapshot reads each cell" $
        runQ (snapshot board) !!! c 2 3 @?= Slot (Just X),
      testCase "snapshot covers the whole board" $
        length (filter (/= Slot Nothing) (toList (runQ (snapshot board)))) @?= 3
    ]
