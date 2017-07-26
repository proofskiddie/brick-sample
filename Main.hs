{-# LANGUAGE TemplateHaskell #-}

import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)

import Brick ( App(..), AttrMap, withBorderStyle, Widget
             , BrickEvent(..), neverShowCursor, EventM
             , Next, AttrName, (<+>), str, padAll, vBox
             , hBox, continue, halt, attrMap, withAttr
             , on, attrName, defaultMain, customMain )

import Control.Concurrent (forkIO)
import Control.Monad (forever, void, guard)
import Data.Maybe (fromMaybe)

import qualified Brick.Widgets.Border as B 
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.BChan (newBChan, writeBChan)

import qualified Graphics.Vty as V


--Game State
data Game = Game 
  {  _loc      :: Coord
    , _dir     :: Direction
    , _frozen  :: Bool
    , _end     :: Bool
  } deriving (Show)

type Coord = V2 Int

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game
makeLenses ''V2

height, width :: Int
height = 40
width  = 200

step :: Game -> Game
step g = fromMaybe g $ do 
--         guard (g ^. frozen == False)
         return $ move g

move :: Game -> Game
move g = g & fun g
  where 
    fun g = case (g ^. dir) of
              North -> (loc . _y %~ (\y -> (y + 1) `mod` height))
              South -> (loc . _y %~ (\y -> (y - 1) `mod` height))
              East  -> (loc . _x %~ (\x -> (x + 1) `mod` width))
              West  -> (loc . _x %~ (\x -> (x - 1) `mod` width))

initGame :: IO Game
initGame =  return $ move $ Game (V2 xm ym) North False False
  where
     xm = width `div` 2
     ym = height `div` 2 


---UI

data Tick = Tick

type Name = ()

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

drawUI :: Game -> [Widget ()]
drawUI g = [ C.center $ drawGrid g ]

drawBorder :: Widget ()
drawBorder = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Derp") (str "  ")

drawGrid :: Game -> Widget ()
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Derp") 
  $ vBox rows
  where 
    rows         = [hBox $ cellsInRow r | r <- [height-1, height-2..0]]
    cellsInRow y = [drawCoord (V2 x y)  | x <- [0..width-1]]
    drawCoord    = cellAt
    cellAt c
      | c == g ^. loc = withAttr blockAttr (guy (g ^. loc . _x) (g ^. loc . _y))
      | otherwise     = withAttr emptyAttr (str " ")

guy :: Int -> Int ->  Widget ()
guy n m = (str $ show n) <+> (str $ show m)

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g _ | ((\x y -> x <= 20 && x >= 15 && y == 39) (g ^. loc . _x) (g ^. loc. _y)) 
                                                  = continue $ g & (loc .~ (V2 10 10))
handleEvent g (AppEvent Tick) = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))       = continue $ g & (dir .~ North) 
handleEvent g (VtyEvent (V.EvKey V.KDown []))     = continue $ g & (dir .~ South)
handleEvent g (VtyEvent (V.EvKey V.KRight []))    = continue $ g & (dir .~ East)
handleEvent g (VtyEvent (V.EvKey V.KLeft []))     = continue $ g & (dir .~ West)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g 
handleEvent g _                                   = continue $ g

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blockAttr, V.black `on` V.white) ]
    
blockAttr, emptyAttr :: AttrName
blockAttr = attrName "blockAttr"
emptyAttr = attrName "emptyAttr"

--Main

main :: IO ()
main = do
  chan <- newBChan 1
  forkIO $ forever $ do
    writeBChan chan Tick
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g
