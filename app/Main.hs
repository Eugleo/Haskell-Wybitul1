{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Brick.AttrMap        (AttrMap, attrMap)
import           Brick.Main           (App (..), continue, defaultMain, halt,
                                       resizeOrQuit, showFirstCursor)
import           Brick.Types          (BrickEvent (VtyEvent), EventM,
                                       Location (..), Next, Padding (..),
                                       Widget)
import           Brick.Widgets.Border as B
import           Brick.Widgets.Core   (hBox, padAll, padBottom, padLeft,
                                       padLeftRight, padRight, padTop,
                                       padTopBottom, showCursor, str, vBox,
                                       withAttr)

import           Control.Monad        (void)
import           Conway               (conwaysRule, mkGrid, step,
                                       toggleCellRule)
import           Data.Function        (on)
import qualified Graphics.Vty         as V
import           Store                (Store (..), experiment, restore, seek)
import           System.Environment   (getArgs)
import           Utils                (chunksOf, wrapAround)

type Grid = Store Coord Bool

type Coord = (Int, Int)

data GameObject =
  GO (Int, Int) -- window size
     Grid

app :: App GameObject () ()
app =
  App
    { appDraw = \s -> [drawGrid s]
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const attributeMap
    , appChooseCursor = showFirstCursor
    }

initialState :: GameObject
initialState = GO (30, 30) $ mkGrid []

main :: IO ()
main = void $ defaultMain app initialState

attributeMap :: AttrMap
attributeMap =
  attrMap V.defAttr [("aliveAttr", V.defAttr `V.withBackColor` V.green)]

drawGrid :: GameObject -> Widget ()
drawGrid (GO (w, h) grid) =
  let (cx, cy) = restore grid
   in showCursor () (Location (cx + 1, cy + 1)) $
      B.borderWithLabel (str "Game of Life") $ hBox columns
  where
    cells -- the list comp generater [(0, 0), (0, 1), (0, 2) ...], i.e. by column
     = experiment (const [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]) grid
    columns = map (vBox . map drawCell) (chunksOf h cells)

drawCell :: Bool -> Widget ()
drawCell False = withAttr "deadAttr" (str " ")
drawCell True  = withAttr "aliveAttr" (str " ")

handleEvent :: GameObject -> BrickEvent () () -> EventM () (Next GameObject)
handleEvent (GO (w, h) grid) (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  continue $ GO (w, h) $ step (conwaysRule w h) grid
handleEvent (GO window grid) (VtyEvent (V.EvKey (V.KChar 'x') [])) =
  let (cx, cy) = restore grid
   in continue $ GO window $ step (toggleCellRule cx cy) grid
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent (GO window grid) (VtyEvent (V.EvKey V.KLeft [])) =
  let (cx, cy) = restore grid
      new = wrapAround window (cx - 1, cy)
   in continue $ GO window (seek new grid)
handleEvent (GO window grid) (VtyEvent (V.EvKey V.KRight [])) =
  let (cx, cy) = restore grid
      new = wrapAround window (cx + 1, cy)
   in continue $ GO window (seek new grid)
handleEvent (GO window grid) (VtyEvent (V.EvKey V.KDown [])) =
  let (cx, cy) = restore grid
      new = wrapAround window (cx, cy + 1)
   in continue $ GO window (seek new grid)
handleEvent (GO window grid) (VtyEvent (V.EvKey V.KUp [])) =
  let (cx, cy) = restore grid
      new = wrapAround window (cx, cy - 1)
   in continue $ GO window (seek new grid)
