module Agent where
import Game.Agent
import Objects

type NMEvent = BEvent NMLocation NMCounters NMResource NMPhaseName NMPlayName NMIssue
