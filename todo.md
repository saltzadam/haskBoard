haskboard
======

- visibility :check:
    - needs consistent interface first :check: no it doesn't
- Re-evaluate: why is `play` a type parameter instead of `data Play l cn
  r i = Play (name :: Text) (nodes :: GameNode ...)`
- Re-evaluate: why is `phase` a type parameter instead of `data Phase
  ...`
- Add Turn -> Phase structure :check:
    - Pros: more obvious control flow; game less likely to stall out for
      lack of nodes
    - Cons: possible that some games don't fit but can't think of one.
- Some kind of in-game "announce" effect, or is that only for the
  interface?
- delta log/rewind

games
======

make more games instead of trying to figure out the perfect interface for
cantstop

- No thanks!
- Splendor
    - needs visibility?
- Sushi Go!
- Point Salad

tui
======

How can Haskell communicate with a Python frontend? Can we spawn the
python process in Haskell and send requests etc?
