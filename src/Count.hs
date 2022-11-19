{-# LANGUAGE DeriveFunctor #-}
module Count where
import Control.Applicative (liftA2)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set (Set)

-- This is Maybe a with the opposite order for Nothing
-- Use it for counting things, think about "unlimited" stuff
data Cnt a = Cnt a | Infinity deriving (Eq, Show, Functor)

instance Ord a => Ord (Cnt a) where
    (Cnt a) <= (Cnt b) = a <= b
    Infinity <= (Cnt _) = False
    (Cnt _) <= Infinity = True
    Infinity <= Infinity = True
instance Applicative Cnt where
    pure = Cnt
    (<*>) (Cnt f) (Cnt a) = Cnt (f a)
    (<*>) Infinity _ = Infinity
    (<*>) _ Infinity = Infinity
instance Semigroup a => Semigroup (Cnt a) where
    (<>) = liftA2 (<>)
instance Monoid a => Monoid (Cnt a) where
    mempty = pure mempty

instance Num a => Num (Cnt a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)

    abs Infinity = Infinity
    abs (Cnt a) = Cnt (abs a)

    signum Infinity = 1
    signum (Cnt a) = Cnt (signum a)

    fromInteger = Cnt . fromInteger

    (-) Infinity _ = Infinity
    (-) _ Infinity = error "no negative infinity"
    (-) (Cnt a) (Cnt b) = Cnt ((-) a b)


-- Could do:
-- 1) create finite list type
-- 2) create function from that type to Stack which creates FinStack
-- 3) do not export FinStack
data Stack a = FinStack [a] | InfStack [a] N1 deriving (Eq, Show, Ord, Functor)

instance Foldable Stack where
    foldMap f (FinStack xs) = foldMap f xs
    foldMap f (InfStack xs ys) = foldMap f (xs ++ concat (repeat ys))

-- is this associative? seems like it
instance Semigroup (Stack a) where
    (<>) (FinStack xs) (FinStack ys) = FinStack (xs <> ys)
    (<>) (FinStack xs) (InfStack ys y) = InfStack (xs <> ys) y
    (<>) (InfStack xs x) _ = InfStack xs x

-- should capture "
-- sDiff :: Stack a -> Stack a -> Stack a



instance Monoid (Stack a) where
    mempty = FinStack []

-- The applicative instance is bad. The Cartesian product of infinite lists with finite head is not
-- infinite with finite head! Could use ZipList instead of it's useful.
-- instance Applicative Stack where
--     pure x = FinStack [x]
--     (<*>) inffs (FinStack xs) = FinStack ((<*>) (toList inffs) xs)
--     (<*>) (FinStack fs) xs = FinStack ((<*>) fs (toList xs))
--     (<*>) fs xs = InfStack ((<*>) (toList fs) (toList xs))

catMaybeStack :: Stack (Maybe a) -> Stack a
catMaybeStack (FinStack as) = FinStack (catMaybes as)
catMaybeStack (InfStack as bs) = case InfStack (catMaybes as) (catMaybes bs)

histogram' :: Ord a => [a] -> Map a (Cnt Int)
histogram' xs = go xs M.empty where
    go (x:xs) aMap = go xs (M.insertWith (+) x 1 aMap) 
    go [] aMap = aMap

histogram :: Ord a => Stack a -> Map a (Cnt Int)
histogram (FinStack xs) = histogram' xs
histogram (InfStack xs x) = M.insert x Infinity (histogram' xs)

pile :: Cnt Int -> a -> Stack a
pile (Cnt i) a = FinStack (replicate i a)
pile Infinity a = InfStack [] a

single :: a -> Stack a
single = pile 1

bottomless :: a -> Stack a
bottomless = InfStack []

stackHead :: Stack a -> Stack a
stackHead (FinStack xs) = FinStack xs
stackHead (InfStack xs _) = FinStack xs

remove :: Eq a => Int -> a -> Stack a -> Stack a
remove _ _ (FinStack []) = FinStack []
remove i a (FinStack (x:xs)) 
  | i <= 0 = FinStack (x:xs)
  | a == x = remove (i-1) a (FinStack xs)
  | otherwise = single x <> remove i a (FinStack xs)
remove i a (InfStack xs y) = remove i a (FinStack xs) <> InfStack [] y
                           

