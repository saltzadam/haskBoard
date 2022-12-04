{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Splendor where

import Location
import Data.Map (Map)
import GHC.Generics (Generic)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Util (compose, enumConstMap, enumerateFromRoot)
import Control.Lens ((&))

data CardTier = TierOne | TierTwo | TierThree | TierFour deriving (Eq, Ord, Show, Enum, Generic, Bounded)
data GemColor = Black | Blue | Green | Red | White | Gold deriving (Eq, Ord, Show, Enum, Generic, Bounded)
data BoardPosition = BPosOne | BPosTwo | BPosThree | BPosFour deriving (Eq, Ord, Show, Enum, Generic, Bounded)
data Player = PlayerOne | PlayerTwo | PlayerThree | PlayerFour deriving (Eq, Ord, Show, Enum, Generic, Bounded)


data SplendorLocation = Hand Player | Tableau Player | Board CardTier BoardPosition | GemPiles | Deck CardTier
    deriving (Eq, Ord, Show, Generic)
data SplendorFResource = Gem GemColor deriving (Eq, Ord, Show, Generic, Bounded)

instance Enum SplendorFResource where
    toEnum = Gem . toEnum
    fromEnum (Gem g) = fromEnum g

data SplendorNFResource = Card CardInfo deriving (Eq, Ord, Show, Generic)



data CardInfo = CardInfo {tier :: CardTier,
                          cost :: Map GemColor Int, -- better as Array or something? Or wait for finitary
                          points :: Int,
                          produces :: Map GemColor Int} deriving (Eq, Ord, Show)


fakeCards :: Map CardTier [CardInfo]
fakeCards = M.empty

allCards :: [CardInfo]
allCards = concat (M.elems fakeCards)

type SplendorLocations = GameObjects SplendorFResource SplendorNFResource SplendorLocation 

-- somePlayers :: Set Player
-- somePlayers = S.fromList [Player "Schwaid", Player "Justin", Player "Uri"]

-- Can improve this with:
--     finitary?
--     real cards?

initGemPiles :: GameObjects SplendorLocation SplendorFResource n -> GameObjects SplendorLocation SplendorFResource n
initGemPiles = addPile GemPiles (enumConstMap 4)  -- enumConstMap works because Gems are the only fungible resource

initDecks :: [GameObjects SplendorLocation f CardInfo -> GameObjects SplendorLocation f CardInfo]
initDecks = [addFullDeck (Deck tier) (Seq.fromList (fakeCards M.! tier)) | tier <- enumerateFromRoot @CardTier]

initHands :: [GameObjects SplendorLocation f CardInfo -> GameObjects SplendorLocation f CardInfo]
initHands = [addHand (Hand player) (M.fromList [(c, 0) | c <- allCards]) | player <- enumerateFromRoot @Player] 

initTabs :: [GameObjects SplendorLocation SplendorFResource n -> GameObjects SplendorLocation SplendorFResource n]
initTabs = [addPile (Tableau player) (enumConstMap 0) | player <- enumerateFromRoot @Player]

initBoards :: [GameObjects SplendorLocation f CardInfo -> GameObjects SplendorLocation f CardInfo]
initBoards = [addEmptyDeck (Deck tier) (Seq.fromList (fakeCards M.! tier)) | tier <- enumerateFromRoot @CardTier]

initLocs :: GameObjects SplendorLocation SplendorFResource CardInfo
initLocs = compose 
           (initGemPiles : initDecks ++ initHands ++ initTabs ++ initBoards)
           emptyLocs

-- type SplendorPlay = Play SplendorResource SplendorLocation

-- data SplendorPlayTypes =  TakeTokens GemColor GemColor GemColor 
--                         | Take2Tokens GemColor
--                         | BuyCard CardTier Int
--                         | ReserveCard CardTier Int
--                         | PlayReservedCard CardTier Int
--                         | ReserveCardFromDeck CardTier


-- canAfford :: Player -> CardInfo -> SplendorCondition
-- canAfford p cInfo = HasAtLeast (Tableau p) (asResource $ cost cInfo)

-- payForCard :: Player -> CardInfo -> SplendorGameObjects -> [Move SplendorResource SplendorLocation]
-- payForCard p info gobj = [Move (asResource $ cost info) (Tableau p) GemPiles,
--                           Move (asResource $ pile goldNeeded Gold) (Tableau p) GemPiles]
--                          where
--     pGems = getGems $ inventory gobj (Tableau p)
--     deficit = cost info - pGems
--     goldNeeded = sum . histogram $ deficit

-- -- highlights rough ergonomics on stacks
-- buildPlay :: SplendorPlayTypes -> Player -> SplendorPlay
-- buildPlay (TakeTokens c1 c2 c3) p = Play p (HasAtLeast GemPiles (g1 <> g2 <> g3)) (const [Move g1 GemPiles (Tableau p), Move g2 GemPiles (Tableau p), Move g3 GemPiles (Tableau p)])
--     where g1 = single (Gem c1)
--           g2 = single (Gem c2)
--           g3 = single (Gem c3)
-- buildPlay (Take2Tokens c) p = Play p (HasAtLeast GemPiles (pile 4 gem)) (const [Move (pile 3 gem) GemPiles (Tableau p)])
--     where gem = Gem c
-- buildPlay (BuyCard t i) p = Play p (p `canAfford` cardLookup t i) _
-- buildPlay _ _ = undefined

-- takeTokens :: Either GemColor (GemColor, GemColor, GemColor) -> Player -> SplendorPlay
-- takeTokens (Left c) p = Play p (HasAtLeast GemPiles (pile 4 gem)) (const [Move (pile 3 gem) GemPiles (Tableau p)])
--     where gem = Gem c
-- takeTokens (Right (c1, c2, c3)) p = Play p (HasAtLeast GemPiles (g1 <> g2 <> g3)) (const [Move g1 GemPiles (Tableau p), Move g2 GemPiles (Tableau p), Move g3 GemPiles (Tableau p)])
--     where g1 = single (Gem c1)
--           g2 = single (Gem c2)
--           g3 = single (Gem c3)

-- cardLookup :: CardTier -> Int -> CardInfo
-- cardLookup _ _ = CardInfo undefined undefined undefined undefined


