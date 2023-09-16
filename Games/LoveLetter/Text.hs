module Text where

import Data.Text (Text)
import qualified Data.Text as T
import Objects (Character (..))

icon' :: Character -> String
icon' Princess = "👸"
icon' Countess = "💃"
icon' King = "👑"
icon' Prince = "🤴"
icon' Priest = "🧙"
icon' Baron = "👲"
icon' Handmaid = "👩"
icon' Guard = "💂"

icon :: Character -> Text
icon = T.pack . icon'

instructions :: Text
instructions =
  T.pack $
    "On your turn, draw a card and play a card. The cards are ranked in descending order below. At the end of the game, the player with the best card wins. \n\
    \ "
      ++ icon' Princess
      ++ ": you lose.  \n\
         \ "
      ++ icon' Countess
      ++ ": nothing. Must play if other card is "
      ++ icon' King
      ++ " or "
      ++ icon' Prince
      ++ ". \n\
         \ "
      ++ icon' King
      ++ ": swap hands with another player. \n\
         \ "
      ++ icon' Prince
      ++ ": choose a player to discard their card and draw another. \n\
         \ "
      ++ icon' Handmaid
      ++ ": you are untargetable until your next turn. \n\
         \ "
      ++ icon' Baron
      ++ ": compare cards with another player. The player with the worse card loses. \n\
         \ "
      ++ icon' Priest
      ++ ": look at another player's hand. \n\
         \ "
      ++ icon' Guard
      ++ ": guess a player's card. If you're right, they lose."
