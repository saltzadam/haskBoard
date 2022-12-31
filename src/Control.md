What should be the actual control structure of a game? Try to describe a
few:

**Can't Stop**: Player turn starts with a roll, then active player
decides to stop or move. Turn rotates after stop or can't move. Turn
rotation returns temp markers to the box
    
    roll
    player decision
    simple resolve
    cleanup

**Splendor**: Player must choose an action. Then next player. Need to
check for discard and winner between turns.

    player decision
    simple resolve
    cleanup

**Scythe**: Player chooses two actions. Resolution can involve other
players (battle). Stuff between turns.

    player decision
    complex resolve
    player decision
    complex resolve
    cleanup

**GWT**: same as Scythe but without other player choices.

**Point salad**: basically same

**Race**: simultaneously choose phases. Then simultaneously choose
actions within phases. Cleanup (winner) after

    simultaneous decision
    decides structure of rest of turn
    simple (simul) resolution though

**Blood rage**: turns plus complicated resolution.
    don't totally remember lol

**Dominion**: easy
    player decision
    resolution
    cleanup

**Gaia project**: same as scythe (complex resolution from power)

What about a Decision - Choice - Action? Phases are nice though because
that's how the rules read.

In phase x
    do this
    choose this
    resolve
    do this

Phase consists of one big decision, then follow-ups?
Phase {enterAction :: [GameAction],
       choices :: [Condition Play?], (or change Play to Choice)
       cleanUpaction :: [GameAction],
       resolution :: ?
       }

resolution takes a choice (in a phase) and returns some list of
actions/choices

tree like

choice
|- action
|- choice
|- action

traverse this depth-first (always?)

handler Node -> (Actions, more nodes)
keep consuming nodes

