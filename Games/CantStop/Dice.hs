module Dice
    where
import Count

line =  "+-----+"

blank = "|     |"
left =  "|*    |"
right = "|    *|"
mid   = "|  *  |"
two   = "|*   *|"

renderDice :: Cnt Int -> String
renderDice x | x == Cnt 1 = unlines [line, blank, mid, blank, line]
             | x == Cnt 2 = unlines [line, left, blank, right, line] 
             | x == Cnt 3 = unlines [line, left, mid, right, line] 
             | x == Cnt 4 = unlines [line, two, blank, two, line]
             | x == Cnt 5 = unlines [line, two, mid, two, line]
             | x == Cnt 6 = unlines [line, two, two, two, line]
             | otherwise = error "illegal die render"
