{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Splendor where

import Data.Set (Set)
import qualified Data.Set as S
import Count
import Game

data CardTier = TierOne | TierTwo | TierThree | TierFour deriving (Eq, Ord, Show, Enum)
data GemColor = Black | Blue | Green | Red | White | Gold deriving (Eq, Ord, Show, Enum)

data BoardPosition = BPosOne | BPosTwo | BPosThree | BPosFour deriving (Eq, Ord, Show, Enum)

data CardInfo = CardInfo {tier :: CardTier,
                          cost :: Stack GemColor,
                          points :: Int,
                          produces :: Stack GemColor}

cardLookup :: CardTier -> Int -> CardInfo
cardLookup _ _ = CardInfo undefined undefined undefined undefined

class IsResource a where
    asResource :: Stack a -> Stack SplendorResource

instance IsResource GemColor where
    asResource = fmap Gem

instance IsResource CardInfo where
    asResource = fmap Card

data SplendorLocation = Hand Player | Tableau Player | Board CardTier BoardPosition | GemPiles | Deck CardTier
data SplendorResource = Gem GemColor 
                      | Card CardInfo


data SimpleVisibility = All | None deriving (Eq, Ord, Show)

type SplendorObjects = GameObjects SplendorResource SplendorLocation SimpleVisibility

somePlayers :: Set Player
somePlayers = S.fromList [Player "Schwaid", Player "Justin", Player "Uri"]

initialInv :: SplendorLocation -> Stack SplendorResource
initialInv GemPiles = mconcat [pile 4 (Gem c) | c <- [Black .. Gold]]
initialInv (Deck tier') = FinStack [Card $ cardLookup tier' i | i <- [3..10]]
initialInv (Hand _) = mempty
initialInv (Tableau _) = mempty
initialInv (Board _ _) = mempty

splendorVisibility :: SplendorLocation -> Player -> SimpleVisibility
splendorVisibility (Hand p) p' = if p == p' then All else None
splendorVisibility (Deck _) _ = None
splendorVisibility _ _ = All

type SplendorGameObjects = GameObjects SplendorResource SplendorLocation SimpleVisibility
type SplendorCondition = Condition SplendorResource SplendorLocation SimpleVisibility

initialGameObjects :: SplendorGameObjects
initialGameObjects = GameObjects
    { players = somePlayers,
      inventory = initialInv,
      visibility = splendorVisibility
    }

getGems :: Stack SplendorResource -> Stack GemColor
getGems = catMaybeStack  . fmap convertGem where
    convertGem :: SplendorResource -> Maybe GemColor
    convertGem (Gem c) = Just c
    convertGem (Card _) = Nothing

type SplendorPlay = Play SplendorResource SplendorLocation SimpleVisibility

data SplendorPlayTypes =  TakeTokens GemColor GemColor GemColor 
                        | Take2Tokens GemColor
                        | BuyCard CardTier Int
                        | ReserveCard CardTier Int
                        | PlayReservedCard CardTier Int
                        | ReserveCardFromDeck CardTier


canAfford :: Player -> CardInfo -> SplendorCondition
canAfford p cInfo = HasAtLeast (Tableau p) (asResource $ cost cInfo)

payForCard :: Player -> CardInfo -> SplendorGameObjects -> [Move SplendorResource SplendorLocation]
payForCard p info gobj = [Move (asResource $ cost info) (Tableau p) GemPiles,
                          Move (asResource $ pile goldNeeded Gold) (Tableau p) GemPiles]
                         where
    pGems = getGems $ inventory gobj (Tableau p)
    deficit = cost info - pGems
    goldNeeded = sum . histogram $ deficit

-- highlights rough ergonomics on stacks
buildPlay :: SplendorPlayTypes -> Player -> SplendorPlay
buildPlay (TakeTokens c1 c2 c3) p = Play p (HasAtLeast GemPiles (g1 <> g2 <> g3)) (const [Move g1 GemPiles (Tableau p), Move g2 GemPiles (Tableau p), Move g3 GemPiles (Tableau p)])
    where g1 = single (Gem c1)
          g2 = single (Gem c2)
          g3 = single (Gem c3)
buildPlay (Take2Tokens c) p = Play p (HasAtLeast GemPiles (pile 4 gem)) (const [Move (pile 3 gem) GemPiles (Tableau p)])
    where gem = Gem c
buildPlay (BuyCard t i) p = Play p (p `canAfford` cardLookup t i) _
buildPlay _ _ = undefined

takeTokens :: Either GemColor (GemColor, GemColor, GemColor) -> Player -> SplendorPlay
takeTokens (Left c) p = Play p (HasAtLeast GemPiles (pile 4 gem)) (const [Move (pile 3 gem) GemPiles (Tableau p)])
    where gem = Gem c
takeTokens (Right (c1, c2, c3)) p = Play p (HasAtLeast GemPiles (g1 <> g2 <> g3)) (const [Move g1 GemPiles (Tableau p), Move g2 GemPiles (Tableau p), Move g3 GemPiles (Tableau p)])
    where g1 = single (Gem c1)
          g2 = single (Gem c2)
          g3 = single (Gem c3)


