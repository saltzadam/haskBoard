module Agent where

import Game.Agent (BEvent)
import Objects

type LLEvent = BEvent LLLocation LLCounters LLResource LLPhaseName LLPlayName LLIssue
