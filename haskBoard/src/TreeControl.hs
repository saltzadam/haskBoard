{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module TreeControl
    where
import GHC.Generics (Generic)
import Data.Tree (Tree (..))
import Control.Monad.Trans.Except (ExceptT)

data TreeControl b = TContinue -- keep going
                     | TStop -- return this node, then stop computation
                           -- so returns Node child (no other children) and tells Forest to stop
                     -- | TKillBranch  -- return this node, ignore children?
                     | TRestart [b] -- return this node
                                  -- tell Forest to stop other computations and continue instead with these children
                   deriving (Eq, Ord, Show, Generic)

-- TODO: in GameE add runNodeM which will also do the ChangePhaseTo part when binded to `result`.

unfoldTreeControl :: (Monad m) => (b -> m (a, [b], TreeControl b)) -> b -> m (Tree a, TreeControl b)
unfoldTreeControl f b = do
    (a, bs, c) <-  f b
    case  c of
        TContinue -> do
            ts <- unfoldForestControl f bs
            return (Node a ts, TContinue)
        TStop -> return (Node a mempty, TStop) -- will stop entire computation
        TRestart newbs -> return (Node a mempty, TRestart newbs)

            -- mzero <|>  do
            -- ts <- unfoldForestControl f newbs
            -- return (Node a ts)


unfoldForestControl :: forall m a b . (Monad m) => (b -> m (a, [b], TreeControl b)) -> [b] -> m [Tree a]
unfoldForestControl f nodes = fst <$> go (unfoldTreeControl f) TContinue nodes where
    go :: (b -> m (Tree a, TreeControl b))
        -> TreeControl b -- think of this as the control statement from the node to the left
        -> [b]
        -> m ( [Tree a], TreeControl b)
    go _ TStop _ = return ([], TStop) -- if the left node says stop, then stop and tell nodes above
    go _ (TRestart newNodes) _ = return ([], TRestart newNodes) -- if the left node says "we're restarting" then stop and tell nodes above
    go _ _ [] = return ([], TContinue)
    go unfoldTree TContinue (mn:mnodes') = do -- left node says to keep going
        (tree, nextControl) <- unfoldTree mn -- do depth-wise thing
        case nextControl of -- check result of depth-wise thing -- this is the control value BENEATH n
          TContinue -> go unfoldTree TContinue mnodes' -- continue with recursion and pass along 
          TStop -> return ([tree], TStop) -- stop recursion and pass along
          TRestart newbs -> (,TRestart newbs) . fst <$> go unfoldTree TContinue newbs-- start over with new nodes but also pass up TRestart

-- what are we actually trying to do
-- runNode :: GameNode -> Eff es (Either GameControl GameNode)
--
-- Four possibilities
-- Right nodes -- continue depth-first diving
-- Left Continue -- this is a leaf
-- Left End -- terminate and return current state
-- Left ChangePhase -- terminate and then run with these
--
-- handleRun :: Either GameControl GameNode -> Eff es ?
--

