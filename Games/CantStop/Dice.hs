module Dice
    where

line =  "+-----+"

blank = "|     |"
left =  "|*    |"
right = "|    *|"
mid   = "|  *  |"
two   = "|*   *|"

renderDice :: Int -> String
renderDice x | x == 1 = unlines [line, blank, mid, blank, line]
             | x == 2 = unlines [line, left, blank, right, line] 
             | x == 3 = unlines [line, left, mid, right, line] 
             | x == 4 = unlines [line, two, blank, two, line]
             | x == 5 = unlines [line, two, mid, two, line]
             | x == 6 = unlines [line, two, two, two, line]
             | otherwise = error "illegal die render"
