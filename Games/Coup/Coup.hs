{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Coup — complete: the full action set (Income, Foreign Aid, Tax, Coup,
-- Assassinate, Steal, Exchange), the out-of-turn challenge window on every role
-- claim, and the block window (Duke blocks Foreign Aid; Captain/Ambassador
-- block Steal; Contessa blocks Assassinate) — where a block is itself a claim
-- that can be challenged.
module Coup (coup, initGameState, coupRules) where

import qualified Cards
import Control.Monad (filterM, replicateM_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import Game.GameAction (GameAction (..))
import Game.GameState (GameRules (..), GameState (..))
import Game.Player (Player, displayPlayer, mkPlayers)
import Game.Rules
import Game.Visibility (VisData (..), VisibilityMap, hideManyFromAll, makeInvisible)
import Helpers
import Objects
import Util (ifM)

-- Small helpers ----------------------------------------------------------------

pname :: Player -> T.Text
pname = T.pack . displayPlayer

-- | Announce an event tagged with the player it concerns, so per-player UI
-- (the action table / Players block) can attribute it. The global log shows
-- these regardless of the tag.
sayBy :: Player -> T.Text -> CoupM ()
sayBy p msg = act (MakeAnnouncement (Just p) msg)

-- | "1 coin" / "3 coins".
coinsWord :: Int -> T.Text
coinsWord n = T.pack (show n ++ if n == 1 then " coin" else " coins")

tpack :: String -> T.Text
tpack = T.pack

-- | Build an options prompt for a specific player from a (non-empty) play list.
opts :: Player -> [CoupPlayName] -> CoupOptions
opts p plays = baseOptions p (NES.fromList (NE.fromList plays))

-- Coins ------------------------------------------------------------------------

-- Coins are a bounded per-player counter. increment/decrement respect the
-- counter's (0, coinCap) bounds (out-of-range changes are no-ops), so these
-- never over/underflow given the action economy.
gainCoins :: Player -> Int -> CoupM ()
gainCoins p n = replicateM_ n (act (IncrementCounter (PlayerCoins p)))

payCoins :: Player -> Int -> CoupM ()
payCoins p n = replicateM_ n (act (DecrementCounter (PlayerCoins p)))

coinsOf :: Player -> CoupM Int
coinsOf p = lookCounterVal (PlayerCoins p)

-- Influence --------------------------------------------------------------------

-- | Distinct roles a player currently holds face-down. Filtered by actual
-- count: a 'Pile' keeps a zero-count entry after its last card is removed
-- (moveFromL only decrements), so we must not report those phantom roles.
influenceRoles :: Player -> CoupM [Role]
influenceRoles p = filterM (fmap (> 0) . howManyAt (Influence p) . RoleCard) allRoles

-- | Total face-down cards (counts duplicate roles).
influenceCount :: Player -> CoupM Int
influenceCount p = sum <$> traverse (howManyAt (Influence p) . RoleCard) allRoles

hasInfluence :: Player -> CoupM Bool
hasInfluence p = (> 0) <$> influenceCount p

aliveOthers :: Player -> CoupM [Player]
aliveOthers p = filterM hasInfluence =<< lookOtherPlayers p

-- | Flip one of @p@'s face-down cards face-up (a lost influence).
revealCard :: Player -> Role -> CoupM ()
revealCard p role = do
  transfer (Influence p) (Revealed p) (RoleCard role)
  sayBy p (pname p <> tpack (" loses an influence — their " ++ show role ++ " is turned face up"))

-- | Make @p@ lose an influence: they choose which card when they hold more
-- than one distinct role, otherwise it is forced. Ends the game if this leaves
-- a single survivor.
loseInfluence :: Player -> CoupM ()
loseInfluence p = do
  roles <- influenceRoles p
  case roles of
    [] -> justDoNothing -- already out
    [only] -> revealCard p only
    (r0 : _ : _) -> do
      chosen <- makeChoice (opts p (map Reveal roles))
      case chosen of
        Reveal role -> revealCard p role
        _ -> revealCard p r0
  checkElimination p
  checkWin

checkElimination :: Player -> CoupM ()
checkElimination p =
  ifM
    (hasInfluence p)
    justDoNothing
    (sayBy p (pname p <> T.pack " has been eliminated!"))

-- | End the game the moment a single player still holds influence.
checkWin :: CoupM ()
checkWin = do
  survivors <- filterM hasInfluence =<< lookPlayers
  case survivors of
    [w] -> sayBy w (pname w <> T.pack " wins the game!") >> endGame [w]
    _ -> justDoNothing

-- Challenges -------------------------------------------------------------------

-- | Offer every other living player, in turn order, the chance to challenge
-- @claimant@'s claim to @role@. Returns 'True' if the claim stands (nobody
-- challenged, or a challenge failed), 'False' if a challenge succeeded.
challengeWindow :: Player -> Role -> CoupM Bool
challengeWindow claimant role = aliveOthers claimant >>= go
  where
    go [] = return True
    go (c : cs) = do
      dec <- makeChoice (opts c [Challenge, AllowIt])
      case dec of
        Challenge -> resolveChallenge claimant role c
        _ -> go cs

-- | Resolve a challenge: if @claimant@ holds @role@ they prove it (challenger
-- loses influence, claimant swaps the card for a fresh one) and the claim
-- stands; otherwise the claimant loses influence and the claim fails.
resolveChallenge :: Player -> Role -> Player -> CoupM Bool
resolveChallenge claimant role challenger = do
  proves <- has (Influence claimant) (RoleCard role)
  sayBy challenger (pname challenger <> tpack " CHALLENGES " <> pname claimant <> tpack ("'s " ++ show role ++ " claim!"))
  if proves
    then do
      sayBy claimant (pname claimant <> tpack (" reveals a genuine " ++ show role ++ " — the challenge fails; they draw a new card"))
      -- claimant returns the proven card, reshuffles, draws a replacement
      transfer (Influence claimant) CourtDeck (RoleCard role)
      shuffle CourtDeck
      Cards.draw CourtDeck (Influence claimant)
      sayBy challenger (pname challenger <> tpack " pays for the failed challenge")
      loseInfluence challenger
      return True
    else do
      sayBy claimant (pname claimant <> tpack (" had no " ++ show role ++ " — the bluff is caught!"))
      loseInfluence claimant
      return False

-- Blocks -----------------------------------------------------------------------

-- | Offer each potential blocker, in order, the chance to block by claiming a
-- role. A block is itself a claim, so it passes through a challenge window (the
-- acting player and everyone else may challenge it). Returns 'True' if the
-- action ends up blocked; a block defeated by challenge lets the next eligible
-- blocker try, and if none succeed the action proceeds.
blockWindow :: [Player] -> [(CoupPlayName, Role)] -> CoupM Bool
blockWindow [] _ = return False
blockWindow (b : bs) blockOpts = do
  alive <- hasInfluence b
  if not alive
    then blockWindow bs blockOpts
    else do
      dec <- makeChoice (opts b (AllowIt : map fst blockOpts))
      case lookup dec blockOpts of
        Nothing -> blockWindow bs blockOpts -- allowed it
        Just role -> do
          sayBy b (pname b <> tpack (" blocks, claiming " ++ show role))
          blockStands <- challengeWindow b role
          if blockStands then return True else blockWindow bs blockOpts

-- Actions ----------------------------------------------------------------------

stealCoins :: Player -> Player -> CoupM ()
stealCoins thief victim = do
  available <- coinsOf victim
  let amount = min 2 available
  replicateM_ amount (act (DecrementCounter (PlayerCoins victim)))
  replicateM_ amount (act (IncrementCounter (PlayerCoins thief)))
  sayBy thief (pname thief <> tpack " steals " <> coinsWord amount <> tpack " from " <> pname victim)

-- | Ambassador exchange: draw two, then return two (player's choice) to the
-- deck, keeping the same number of cards as before.
doExchange :: Player -> CoupM ()
doExchange p = do
  Cards.draw CourtDeck (Influence p)
  Cards.draw CourtDeck (Influence p)
  returnOne p
  returnOne p
  shuffle CourtDeck
  sayBy p (pname p <> tpack " exchanges cards with the court deck")
  where
    returnOne pl = do
      roles <- influenceRoles pl
      case roles of
        [] -> justDoNothing
        [only] -> transfer (Influence pl) CourtDeck (RoleCard only)
        (r0 : _ : _) -> do
          chosen <- makeChoice (opts pl (map Reveal roles))
          let role = case chosen of Reveal r -> r; _ -> r0
          transfer (Influence pl) CourtDeck (RoleCard role)

-- | The active player's menu of legal actions.
chooseAction :: Player -> CoupM ()
chooseAction p = do
  coins <- coinsOf p
  targets <- aliveOthers p
  stealTargets <- filterM (fmap (> 0) . coinsOf) targets
  let coupPlays = [LaunchCoup t | coins >= 7, t <- targets]
      assassinatePlays = [Assassinate t | coins >= 3, t <- targets]
      stealPlays = [Steal t | t <- stealTargets]
      plays
        | coins >= 10 = coupPlays -- 10+ coins: Coup is mandatory
        | otherwise =
            [Income, ForeignAid, TakeTax, ExchangeCards]
              ++ coupPlays
              ++ assassinatePlays
              ++ stealPlays
  makeChoice_ (opts p plays)

coupRunPlay :: CoupPlayName -> CoupM ()
coupRunPlay Income = activePlayer $ \p -> do
  gainCoins p 1
  sayBy p (pname p <> tpack " takes Income (+1 coin)")
coupRunPlay ForeignAid = activePlayer $ \p -> do
  sayBy p (pname p <> tpack " attempts Foreign Aid")
  others <- aliveOthers p
  blocked <- blockWindow others [(BlockForeignAid, Duke)] -- any Duke can block
  if blocked
    then sayBy p (pname p <> tpack "'s Foreign Aid is blocked")
    else do
      gainCoins p 2
      sayBy p (pname p <> tpack " collects " <> coinsWord 2 <> tpack " (Foreign Aid)")
coupRunPlay TakeTax = activePlayer $ \p -> do
  sayBy p (pname p <> tpack " claims Duke and attempts Tax")
  stands <- challengeWindow p Duke
  if stands
    then do
      gainCoins p 3
      sayBy p (pname p <> tpack " collects " <> coinsWord 3 <> tpack " (Tax)")
    else justDoNothing -- the caught bluff was already announced
coupRunPlay (LaunchCoup target) = activePlayer $ \p -> do
  payCoins p 7
  sayBy p (pname p <> tpack " pays " <> coinsWord 7 <> tpack " and launches a Coup against " <> pname target)
  loseInfluence target
coupRunPlay (Assassinate target) = activePlayer $ \p -> do
  payCoins p 3 -- paid whether or not it lands
  sayBy p (pname p <> tpack " pays " <> coinsWord 3 <> tpack " and claims Assassin against " <> pname target)
  stands <- challengeWindow p Assassin
  targetAlive <- hasInfluence target
  if stands && targetAlive
    then do
      blocked <- blockWindow [target] [(BlockAssassination, Contessa)]
      if blocked
        then sayBy p (pname p <> tpack "'s assassination of " <> pname target <> tpack " is blocked")
        else do
          sayBy target (pname target <> tpack " is assassinated!")
          loseInfluence target
    else justDoNothing
coupRunPlay (Steal target) = activePlayer $ \p -> do
  sayBy p (pname p <> tpack " claims Captain and attempts to steal from " <> pname target)
  stands <- challengeWindow p Captain
  targetAlive <- hasInfluence target
  if stands && targetAlive
    then do
      blocked <- blockWindow [target] [(BlockStealCaptain, Captain), (BlockStealAmbassador, Ambassador)]
      if blocked
        then sayBy p (pname p <> tpack "'s theft from " <> pname target <> tpack " is blocked")
        else stealCoins p target
    else justDoNothing
coupRunPlay ExchangeCards = activePlayer $ \p -> do
  sayBy p (pname p <> tpack " claims Ambassador and attempts to Exchange")
  stands <- challengeWindow p Ambassador
  if stands then doExchange p else justDoNothing
-- reaction / sub-choice plays resolve to nothing; orchestration code branches
-- on the value returned from 'makeChoice' instead.
coupRunPlay _ = justDoNothing

-- Turn flow --------------------------------------------------------------------

-- | Pass the turn to the next player (after the current one, cyclically) who
-- still holds influence. Robust to the current player having just eliminated
-- themselves via a failed bluff.
advanceToNextAlive :: CoupM ()
advanceToNextAlive = do
  ps <- lookPlayers
  current <- lookCurrentTurnOwner
  alive <- filterM hasInfluence ps
  case filter (> current) alive ++ filter (< current) alive of
    (nxt : _) -> advanceTurn (playerTurn nxt)
    [] -> justDoNothing -- only the current player remains; checkWin ends it

-- Initialization ---------------------------------------------------------------

-- | You see your own face-down cards; the court deck is hidden from all.
coupVisibility :: [Player] -> VisibilityMap CoupLocation CoupCounter
coupVisibility players =
  let hideCourt = hideManyFromAll players [VisLocation CourtDeck]
      pairs = [(owner, other) | owner <- players, other <- players, owner /= other]
      hideOne vm (owner, other) = makeInvisible vm other (VisLocation (Influence owner))
   in foldl hideOne hideCourt pairs

coupSetup :: CoupM ()
coupSetup = do
  shuffle CourtDeck
  players <- lookPlayers
  Cards.dealNTo 2 CourtDeck Influence players

score :: Player -> CoupM Int
score p = ifM (hasInfluence p) 1 0

coupPhases :: CoupPhaseName -> CoupPhase
coupPhases name@(CoupTurn _) =
  mkPhase name (activePlayer chooseAction >> advanceToNextAlive)

initGameState :: Int -> CoupGameState
initGameState numPlayers =
  let players = mkPlayers numPlayers
      pset = S.fromList players
      first = minBound -- Player PlayerOne, always the first seat
   in GameState
        { players = pset,
          objects = initGameObjects pset,
          currentPhase = CoupTurn first,
          currentTurn = playerTurn first,
          nextTurn = playerTurn first, -- overwritten by advanceToNextAlive
          visibility = coupVisibility players
        }

coupRules :: CoupGameRules
coupRules =
  GameRules
    { playRunner = coupRunPlay,
      phases = coupPhases,
      score = score,
      scoreBounds = (0, 1),
      scorePublic = True,
      setupPhase = Just coupSetup
    }

coup :: Int -> (CoupGameState, CoupGameRules)
coup n = (initGameState n, coupRules)
