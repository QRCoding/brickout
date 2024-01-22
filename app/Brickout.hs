{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Brickout where

import Control.Lens
import GHC.Float (int2Float)
import Linear.V2

-- Configuration

height, width :: Int
height = 60
width = 63

brickWidth, brickHeight :: Int
brickWidth = 5
brickHeight = 1

paddleSize, moveDistance :: Int
paddleSize = 6
moveDistance = 4

-- Types

data Dir = L | R
type Brick = V2 Int
type Bricks = [Brick]

data Game = Game
    { _paddlePos :: !Int
    , _bricks :: !Bricks
    , _ballPos :: !(V2 Float)
    , _ballDir :: !(V2 Float)
    }

makeLenses ''Game

-- Initial state

initGame :: Game
initGame =
    Game
        (width `div` 2)
        initBricks
        initBallPos
        (V2 0 -1)

initBallPos :: V2 Float
initBallPos = V2 (int2Float height / 2) (int2Float width / 2)

-- Brick configuration

initBricks :: Bricks
initBricks =
    [ V2 x y
    | y <- [start, start + 3 .. height - 1]
    , x <- [2, 8 .. width - 2]
    ]
  where
    start = height `div` 2

inBrick :: V2 Int -> Brick -> Bool
inBrick (V2 x y) (V2 bx by) =
    y `elem` [by .. by + brickHeight - 1]
        && x `elem` [bx .. bx + brickWidth - 1]

-- Logic

move :: Dir -> Game -> Game
move L = paddlePos %~ clamp . subtract moveDistance
move R = paddlePos %~ clamp . (+ moveDistance)

clamp :: Int -> Int
clamp p
    | p <= 0 = 0
    | p + paddleSize >= width = width - paddleSize
    | otherwise = p

inPaddle :: V2 Int -> Int -> Bool
inPaddle (V2 x y) p = (x >= p && x <= rightEdge) && y == 2
  where
    rightEdge = p - 1 + paddleSize
