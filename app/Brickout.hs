{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TemplateHaskell #-}

module Brickout where

import Control.Lens
import GHC.Float (int2Float)
import Linear ((^+^))
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
moveDistance = 2

maxAngle :: Float
maxAngle = 4 * pi / 12

-- Types

data Bound = BL | BR | BU | BD

data Dir = L | R

type Brick = V2 Int

type Bricks = [Brick]

data Game = Game
  { _paddlePos :: !Int,
    _bricks :: !Bricks,
    _ballPos :: !(V2 Float),
    _ballDir :: !(V2 Float)
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
    | y <- [start, start + 3 .. height - 1],
      x <- [2, 8 .. width - 2]
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

step :: Game -> Game
step g =
  g
    & ballDir .~ v
    & ballPos %~ (^+^ v)
  where
    v =
      checkPaddle p w
      . flipVelocity (view ballPos g)
        $ view ballDir g
    w = view paddlePos g
    p = view ballPos g

checkPaddle :: V2 Float -> Int -> V2 Float -> V2 Float
checkPaddle p w v
  | inPaddle (round <$> p ^+^ v) w
      || inPaddle (round <$> p) w = V2 (-sin a) (cos a)
  | otherwise = v
  where
    intersect = int2Float w + int2Float paddleSize / 2 - p ^. _x
    intersectN = intersect / (int2Float paddleSize / 2)
    a = intersectN * maxAngle

flipVelocity :: V2 Float -> V2 Float -> V2 Float
flipVelocity p v = case outOfBounds p of
  Just d -> modifyVelocity d v
  Nothing -> v

outOfBounds :: V2 Float -> Maybe Bound
outOfBounds (V2 x y)
  | int2Float height < y + 1 = Just BU
  | 0 > y = Just BD
  | 0 > x = Just BL
  | int2Float width < x + 1 = Just BR
  | otherwise = Nothing

modifyVelocity :: Bound -> V2 Float -> V2 Float
modifyVelocity BU = _y %~ (* -1) . abs
modifyVelocity BD = _y %~ abs
modifyVelocity BL = _x %~ abs
modifyVelocity BR = _x %~ (* -1) . abs
