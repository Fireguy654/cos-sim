module Data.Grid
  ( Grid (..)
  , gUp
  , gDown
  , gLeft
  , gRight
  , gWrite
  , gModify
  , fromLZ
  , toMatrix
  ) where

import           Control.Comonad (Comonad (..))

import           Data.ListZipper (ListZipper (..), genericMove, listLeft,
                                  listRight, listWrite, toList)

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

gUp, gDown, gLeft, gRight :: Grid a -> Grid a
gUp (Grid g) = Grid $ listLeft g
gDown (Grid g) = Grid $ listRight g
gLeft (Grid g) = Grid $ listLeft <$> g
gRight (Grid g) = Grid $ listRight <$> g

gWrite :: a -> Grid a -> Grid a
gWrite a (Grid g) = Grid $ listWrite (listWrite a $ extract g) g

gModify :: (a -> a) -> Grid a -> Grid a
gModify f g = gWrite (f $ extract g) g

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove gLeft gRight
vertical = genericMove gUp gDown

fromLZ :: ListZipper a -> Grid a
fromLZ = Grid . duplicate

toMatrix :: Grid a -> Int -> [[a]]
toMatrix (Grid g) sz = toList ((`toList` sz) <$> g) sz

instance Functor Grid where
  fmap f = Grid . fmap (fmap f) . unGrid

instance Comonad Grid where
  extract = extract . extract . unGrid

  duplicate = Grid . fmap horizontal . vertical
