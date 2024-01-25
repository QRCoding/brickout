module UI (main) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brickout
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Linear.V2

-- Types

data Tick = Tick

type Name = ()

data Cell = Paddle | Ball | Brick | Empty

-- App

app :: App Game Tick Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

theMap :: AttrMap
theMap =
    attrMap
        V.defAttr
        [ (paddleAttr, V.blue `on` V.blue)
        , (brickAttr, V.red `on` V.red)
        , (ballAttr, V.green `on` V.green)
        ]

drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ drawGrid g]

drawGrid :: Game -> Widget Name
drawGrid g =
    withBorderStyle BS.unicodeBold
        . B.borderWithLabel (str "Brickout")
        $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height - 1, height - 2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
        | inPaddle c (g ^. paddlePos) = Paddle
        | any (inBrick c) $ g ^. bricks = Brick
        | c == (round <$> g ^. ballPos) = Ball
        | otherwise = Empty

drawCell :: Cell -> Widget Name
drawCell Paddle = withAttr paddleAttr cw
drawCell Empty = withAttr emptyAttr cw
drawCell Brick = withAttr brickAttr cw
drawCell Ball = withAttr ballAttr cw

cw :: Widget Name
cw = str " "

paddleAttr, emptyAttr, brickAttr, ballAttr :: AttrName
paddleAttr = attrName "paddle"
emptyAttr = attrName "empty"
brickAttr = attrName "brick"
ballAttr = attrName "ball"

-- Event handling

handleEvent :: BrickEvent Name Tick -> EventM Name Game ()
handleEvent e = do
    case e of
        AppEvent Tick -> modify step
        VtyEvent vtye ->
            case vtye of
                V.EvKey (V.KChar 'a') [] -> modify $ move L
                V.EvKey (V.KChar 'd') [] -> modify $ move R
                V.EvKey (V.KChar 'q') [] -> halt
                V.EvKey (V.KChar 'r') [] -> modify $ const initGame
                -- V.EvKey (V.KChar 'p') [] -> modify $ paused %~ not
                _other -> continueWithoutRedraw
        _other -> continueWithoutRedraw

-- Main

main :: IO ()
main = do
    chan <- newBChan 10
    _ <- forkIO . forever $ do
        writeBChan chan Tick
        threadDelay 20000
    let g = initGame
    let builder = mkVty V.defaultConfig
    initialVty <- builder
    void $ customMain initialVty builder (Just chan) app g
