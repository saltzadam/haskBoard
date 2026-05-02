module Brick.Widgets.TextStream where

import Brick
import Data.Text (Text)
import qualified Graphics.Vty as V
import Text.Wrap

renderBottomUp :: [Widget n] -> Widget n
renderBottomUp ws =
  Widget Greedy Greedy $ do
    let go _ [] = return V.emptyImage
        go remainingHeight (c : cs) = do
          cResult <- render c
          let img = image cResult
              newRemainingHeight = remainingHeight - V.imageHeight img
          if newRemainingHeight == 0
            then return img
            else
              if newRemainingHeight < 0
                then return $ V.cropTop remainingHeight img
                else do
                  rest <- go newRemainingHeight cs
                  return $ V.vertCat [rest, img]

    ctx <- getContext
    img <- go (availHeight ctx) ws
    render $ fill ' ' <=> raw img

textStream :: (Ord n, Show n) => n -> Int -> Int -> [Text] -> Widget n
textStream name hsize vsize lineWidgets = vLimit vsize . hLimit hsize $ viewport name Vertical (vLimit vsize . hLimit hsize . renderBottomUp $ txtWrapWith settings <$> lineWidgets)
  where
    settings =
      WrapSettings
        { preserveIndentation = True,
          breakLongWords = False,
          fillStrategy = FillIndent 4,
          fillScope = FillAll
        }
