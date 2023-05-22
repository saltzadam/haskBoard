-- This is taken from Swarm.TUI.Panel, maintained by Brent Yorgey
-- Guess that means we're BSD-3?
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Panel where

import Brick (Named (..), Widget, overrideAttr)
import Brick.Focus (FocusRing, withFocusRing)
import Brick.Widgets.Border (border, borderAttr)
import Control.Lens (makeFields, view)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.OverloadedLabels (IsLabel (..))


data Panel n = Panel
  {panelName :: n, panelContent :: Widget n} deriving (Generic)

instance HasField "panelName" (Panel n) n => IsLabel "panelName" (Panel n -> n) where
  fromLabel = getField @"panelName"

instance HasField "panelContent" (Panel n) (Widget n) => IsLabel "panelContent" (Panel n -> Widget n) where
  fromLabel = getField @"panelContent"



makeFields ''Panel

instance Named (Panel n) n where
  getName = #panelName

drawPanel :: Eq n => FocusRing n -> Panel n -> Widget n
drawPanel fr = withFocusRing fr drawPanel'
  where
    drawPanel' :: Bool -> Panel n -> Widget n
    drawPanel' focused =
      -- (if focused then overrideAttr focusedBorder borderAttr else overrideAttr borderAttr panelBorderAttr) .
          border
        . #panelContent

-- | Create a panel.
panel ::
  Eq n =>
  -- | Focus ring the panel should be part of.
  FocusRing n ->
  -- | The name of the panel. Must be unique.
  n ->
  -- | The content of the panel.
  Widget n ->
  Widget n
panel fr nm w = drawPanel fr (Panel nm w)
