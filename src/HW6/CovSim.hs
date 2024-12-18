{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid
  , cellSymb
  , simulate
  ) where

import           Control.Comonad (extend, extract)
import           Control.Monad   (liftM2)
import           Data.Grid       (Grid (..), fromLZ, gDown, gLeft, gModify,
                                  gRight, gUp)
import           Data.ListZipper (ListZipper (LZ))
import           Lens.Micro      ((%~), (&), (.~), (^.))
import           Lens.Micro.TH   (makeLenses)
import           System.Random   (Random (random, randoms), RandomGen (split),
                                  StdGen, mkStdGen)

data Config = Config
  { _probability      :: Double
  , _incubationPeriod :: Int
  , _illnessDuration  :: Int
  , _immunityDuration :: Int
  } deriving Show

makeLenses ''Config

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

pushTime :: CellState -> CellState
pushTime Healthy      = Healthy
pushTime (Infected n) = Infected $ n - 1
pushTime (Ill n)      = Ill $ n - 1
pushTime (Immune n)   = Immune $ n - 1

isDeseased :: CellState -> Bool
isDeseased Healthy    = False
isDeseased (Immune _) = False
isDeseased _          = True

data Cell = Cell
  { _cellState :: CellState
  , _cellRand  :: StdGen
  }

makeLenses ''Cell

cellSymb :: Cell -> Char
cellSymb cell = case cell^.cellState of
  Healthy      -> '_'
  (Infected _) -> 'i'
  (Ill _)      -> '#'
  Immune _     -> '@'

mkCell :: (CellState, StdGen) -> Cell
mkCell (st, gen) = Cell { _cellState = st , _cellRand = gen }

iterCell :: Config -> Cell -> Cell
iterCell conf cell = case cell^.cellState of
  Healthy    -> cell & cellState .~ Infected (conf^.incubationPeriod - 1)
  Infected 0 -> cell & cellState .~ Ill (conf^.illnessDuration - 1)
  Ill 0      -> cell & cellState .~ Immune (conf^.immunityDuration - 1)
  Immune 0   -> cell & cellState .~ Healthy
  _          -> cell & cellState %~ pushTime


type Comonad19Grid = Grid Cell

initGrid :: Int -> Config -> Comonad19Grid
initGrid seed conf = let
  (genLeft, genRight) = split $ mkStdGen seed
  (randomsLeft, randomsRight) = (randoms genLeft, randoms genRight)
  in gModify (iterCell conf) (mkCell . (Healthy,) . mkStdGen <$> fromLZ (LZ (tail randomsLeft) (head randomsLeft) randomsRight))

neighboors :: [Comonad19Grid -> Comonad19Grid]
neighboors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [gLeft, gRight]
        verticals = [gUp, gDown]

neighboorCollect :: Config -> Cell -> (Bool, StdGen) -> (Bool, StdGen)
neighboorCollect _ _ res@(True, _) = res
neighboorCollect conf cell (False, gen) = let (r, newGen) = random gen
  in if isDeseased $ cell^.cellState then (r < conf^.probability, newGen) else (False, newGen)

rule :: Config -> Comonad19Grid -> Cell
rule conf g = let cell = extract g in case cell^.cellState of
  Healthy -> let
    (inf, newGen) = foldr (neighboorCollect conf . extract . ($ g)) (False, cell^.cellRand) neighboors
    newCell = if inf then iterCell conf cell else cell
    in newCell & cellRand .~ newGen
  _ -> iterCell conf cell

covStep :: Config -> Comonad19Grid -> Comonad19Grid
covStep conf = extend (rule conf)

simulate :: Int -> Config -> [Comonad19Grid]
simulate seed config = iterate (covStep config) (initGrid seed config)
