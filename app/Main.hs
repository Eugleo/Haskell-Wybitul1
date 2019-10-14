{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Brick.AttrMap        (AttrMap, AttrName, attrMap)
import           Brick.Main           (App (..), continue, defaultMain, halt,
                                       resizeOrQuit, showFirstCursor)
import           Brick.Types          (BrickEvent (VtyEvent), EventM,
                                       Location (..), Next, Padding (..),
                                       Widget)
import           Brick.Widgets.Border as B
import           Brick.Widgets.Center as C
import           Brick.Widgets.Core   (hBox, padAll, padBottom, padLeft,
                                       padLeftRight, padRight, padTop,
                                       padTopBottom, showCursor, str, vBox,
                                       withAttr)

import           Conway               (conwaysRule, mkGrid, step,
                                       toggleCellRule)
import           Data.Function        (on)
import qualified Graphics.Vty         as V
import           Store                (Store (..), experiment)
import           System.Environment   (getArgs)
import           Utils                (chunksOf)

type Grid = Store Coord Bool

type Coord = (Int, Int)

data GameObject =
  GO (Int, Int) -- window size
     (Int, Int) -- cursor placement
     Grid

app :: App GameObject () ()
app =
  App
    { appDraw = draw
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    , appChooseCursor = showFirstCursor
    }

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [(deadAttr, V.defAttr), (aliveAttr, V.defAttr `V.withBackColor` V.green)]

draw :: GameObject -> [Widget ()]
draw g = [drawGrid g]

drawGrid :: GameObject -> Widget ()
drawGrid (GO (w, h) (cx, cy) grid) =
  showCursor () (Location (cx + 1, cy + 1)) $
  B.borderWithLabel (str "Game of Life") $ hBox columns
  where
    cells -- cells = [(0, 0), (0, 1), (0, 2) ...], i.e. by column
     = experiment (const [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]) grid
    columns = map (vBox . map drawCell) (chunksOf h cells)

drawCell :: Bool -> Widget ()
drawCell False = withAttr deadAttr cw
drawCell True  = withAttr aliveAttr cw

cw :: Widget ()
cw = str " "

deadAttr :: AttrName
deadAttr = "deadAttr"

aliveAttr :: AttrName
aliveAttr = "aliveAttr"

handleEvent :: GameObject -> BrickEvent () () -> EventM () (Next GameObject)
handleEvent (GO (w, h) cursor grid) (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  continue $ GO (w, h) cursor $ step (conwaysRule w h) grid
handleEvent (GO window (cx, cy) grid) (VtyEvent (V.EvKey (V.KChar 'x') [])) =
  continue $ GO window (cx, cy) $ step (toggleCellRule cx cy) grid
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent (GO window (cx, cy) grid) (VtyEvent (V.EvKey V.KLeft [])) =
  continue $ GO window (wrapAround window (cx - 1, cy)) grid
handleEvent (GO window (cx, cy) grid) (VtyEvent (V.EvKey V.KRight [])) =
  continue $ GO window (wrapAround window (cx + 1, cy)) grid
handleEvent (GO window (cx, cy) grid) (VtyEvent (V.EvKey V.KDown [])) =
  continue $ GO window (wrapAround window (cx, cy + 1)) grid
handleEvent (GO window (cx, cy) grid) (VtyEvent (V.EvKey V.KUp [])) =
  continue $ GO window (wrapAround window (cx, cy - 1)) grid

wrapAround :: (Int, Int) -> (Int, Int) -> (Int, Int)
wrapAround (w, h) (x, y) = (x `mod` w, y `mod` h)

main :: IO ()
main = do
  _ <-
    defaultMain
      app
      (GO (30, 30) (0, 0) $ mkGrid [(0, 0), (1, 0), (2, 0), (0, 3), (0, 4)])
  return ()
