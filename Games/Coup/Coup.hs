{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Game.GameState (GameRules (..), GameState (..))
import Game.Player (Player, displayPlayer, mkPlayers)
import Game.Rules
import Game.Visibility (VisData (..), VisibilityMap, hideManyFromAll, makeInvisible)
import Helpers
import Objects
import Util (ifM, tshow)

-- Small helpers ----------------------------------------------------------------

pname :: Player -> T.Text
pname = T.pack . displayPlayer

-- | "1 coin" / "3 coins".
coinsWord :: Int -> T.Text
coinsWord n = tshow n <> if n == 1 then " coin" else " coins"

-- | Build an options prompt for a specific player from a (non-empty) play list.
opts :: Player -> [CoupPlayName] -> CoupOptions
opts p plays = baseOptions p (NES.fromList (NE.fromList plays))

-- Coins ------------------------------------------------------------------------

-- Coins are a bounded per-player counter. increment/decrement respect the
-- counter's (0, coinCap) bounds (out-of-range changes are no-ops), so these
-- never over/underflow given the action economy.
gainCoins :: Player -> Int -> CoupM ()
gainCoins p n = replicateM_ n (incrementCounter (PlayerCoins p))

payCoins :: Player -> Int -> CoupM ()
payCoins p n = replicateM_ n (decrementCounter (PlayerCoins p))

coinsOf :: Player -> CoupM Int
coinsOf p = lookCounterVal (PlayerCoins p)

-- Influence --------------------------------------------------------------------

-- | Distinct roles a player currently holds face-down.
influenceRoles :: Player -> CoupM [Role]
influenceRoles p = map roleOf . S.toList <$> whatsAt (Influence p)

-- | Total face-down cards (counts duplicate roles).
influenceCount :: Player -> CoupM Int
influenceCount p = sum <$> resourcesAt (Influence p)

hasInfluence :: Player -> CoupM Bool
hasInfluence p = (> 0) <$> influenceCount p

aliveOthers :: Player -> CoupM [Player]
aliveOthers p = filterM hasInfluence =<< lookOtherPlayers p

-- | Flip one of @p@'s face-down cards face-up (a lost influence).
revealCard :: Player -> Role -> CoupM ()
revealCard p role = do
  transfer (Influence p) (Revealed p) (RoleCard role)
  announceBy p ("loses an influence — their " <> tshow role <> " is turned face up")

-- | Make @p@ lose an influence: they choose which card when they hold more
-- than one distinct role, otherwise it is forced. Ends the game if this leaves
-- a single survivor. A no-op for a player who is already out.
loseInfluence :: Player -> CoupM ()
loseInfluence p = do
  roles <- influenceRoles p
  case roles of
    [] -> justDoNothing -- already out: nothing to lose or announce
    [only] -> revealCard p only >> afterLoss
    (r0 : _ : _) -> do
      chosen <- makeChoice (opts p (map Reveal roles))
      case chosen of
        Reveal role -> revealCard p role
        _ -> revealCard p r0
      afterLoss
  where
    afterLoss = checkElimination p >> checkWin

checkElimination :: Player -> CoupM ()
checkElimination p =
  ifM
    (hasInfluence p)
    justDoNothing
    (announceBy p "has been eliminated!")

-- | End the game the moment a single player still holds influence.
checkWin :: CoupM ()
checkWin = do
  survivors <- filterM hasInfluence =<< lookPlayers
  case survivors of
    [w] -> announceBy w "wins the game!" >> endGame [w]
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
  announceBy challenger ("CHALLENGES " <> pname claimant <> "'s " <> tshow role <> " claim!")
  if proves
    then do
      announceBy claimant ("reveals a genuine " <> tshow role <> " — the challenge fails; they draw a new card")
      -- claimant returns the proven card, reshuffles, draws a replacement
      transfer (Influence claimant) CourtDeck (RoleCard role)
      shuffle CourtDeck
      Cards.draw CourtDeck (Influence claimant)
      announceBy challenger "pays for the failed challenge"
      loseInfluence challenger
      return True
    else do
      announceBy claimant ("had no " <> tshow role <> " — the bluff is caught!")
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
          announceBy b ("blocks, claiming " <> tshow role)
          blockStands <- challengeWindow b role
          if blockStands then return True else blockWindow bs blockOpts

-- Actions ----------------------------------------------------------------------

stealCoins :: Player -> Player -> CoupM ()
stealCoins thief victim = do
  available <- coinsOf victim
  let amount = min 2 available
  replicateM_ amount (transferCounter (PlayerCoins victim) (PlayerCoins thief))
  announceBy thief ("steals " <> coinsWord amount <> " from " <> pname victim)

-- | Ambassador exchange: draw two, then return two (player's choice) to the
-- deck, keeping the same number of cards as before.
doExchange :: Player -> CoupM ()
doExchange p = do
  Cards.draw CourtDeck (Influence p)
  Cards.draw CourtDeck (Influence p)
  returnOne p
  returnOne p
  shuffle CourtDeck
  announceBy p "exchanges cards with the court deck"
  where
    returnOne pl = do
      roles <- influenceRoles pl
      case roles of
        [] -> justDoNothing
        [only] -> transfer (Influence pl) CourtDeck (RoleCard only)
        (r0 : _ : _) -> do
          chosen <- makeChoice (opts pl (map ReturnCard roles))
          let role = case chosen of ReturnCard r -> r; _ -> r0
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
  announceBy p "takes Income (+1 coin)"
coupRunPlay ForeignAid = activePlayer $ \p -> do
  announceBy p "attempts Foreign Aid"
  others <- aliveOthers p
  blocked <- blockWindow others [(BlockForeignAid, Duke)] -- any Duke can block
  if blocked
    then announceBy p "Foreign Aid is blocked"
    else do
      gainCoins p 2
      announceBy p ("collects " <> coinsWord 2 <> " (Foreign Aid)")
coupRunPlay TakeTax = activePlayer $ \p -> do
  announceBy p "claims Duke and attempts Tax"
  stands <- challengeWindow p Duke
  if stands
    then do
      gainCoins p 3
      announceBy p ("collects " <> coinsWord 3 <> " (Tax)")
    else justDoNothing -- the caught bluff was already announced
coupRunPlay (LaunchCoup target) = activePlayer $ \p -> do
  payCoins p 7
  announceBy p ("pays " <> coinsWord 7 <> " and launches a Coup against " <> pname target)
  loseInfluence target
coupRunPlay (Assassinate target) = activePlayer $ \p -> do
  payCoins p 3 -- paid whether or not it lands
  announceBy p ("pays " <> coinsWord 3 <> " and claims Assassin against " <> pname target)
  stands <- challengeWindow p Assassin
  targetAlive <- hasInfluence target
  if stands && targetAlive
    then do
      blocked <- blockWindow [target] [(BlockAssassination, Contessa)]
      -- a caught Contessa bluff may have eliminated the target already
      stillAlive <- hasInfluence target
      if
        | blocked -> announceBy p ("assassination of " <> pname target <> " is blocked")
        | stillAlive -> do
            announceBy target "is assassinated!"
            loseInfluence target
        | otherwise -> justDoNothing
    else justDoNothing
coupRunPlay (Steal target) = activePlayer $ \p -> do
  announceBy p ("claims Captain and attempts to steal from " <> pname target)
  stands <- challengeWindow p Captain
  targetAlive <- hasInfluence target
  if stands && targetAlive
    then do
      blocked <- blockWindow [target] [(BlockStealCaptain, Captain), (BlockStealAmbassador, Ambassador)]
      -- an eliminated player's coins return to the treasury, so nothing to steal
      stillAlive <- hasInfluence target
      if
        | blocked -> announceBy p ("theft from " <> pname target <> " is blocked")
        | stillAlive -> stealCoins p target
        | otherwise -> justDoNothing
    else justDoNothing
coupRunPlay ExchangeCards = activePlayer $ \p -> do
  announceBy p "claims Ambassador and attempts to Exchange"
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
  current <- lookCurrentTurnOwner
  alive <- filterM hasInfluence =<< lookPlayers
  case nextPlayerAmong current alive of
    Just nxt -> advanceTurn (playerTurn nxt)
    -- Unreachable: when one player remains, checkWin has already called
    -- endGame, which halts the rule before we get here.
    Nothing -> error "Coup.advanceToNextAlive: no other player holds influence"

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
coupPhases name@(CoupTurnPhase _) =
  mkPhase name (activePlayer chooseAction >> advanceToNextAlive)

initGameState :: Int -> CoupGameState
initGameState numPlayers =
  let players = mkPlayers numPlayers
      pset = S.fromList players
      first = minBound -- Player PlayerOne, always the first seat
   in GameState
        { players = pset,
          objects = initGameObjects pset,
          currentPhase = CoupTurnPhase first,
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

-- | The triple 'runGame' expects: initial state, rules, and hints ([] = none).
coup :: Int -> (CoupGameState, CoupGameRules, [CoupHint])
coup n = (initGameState n, coupRules, [])
