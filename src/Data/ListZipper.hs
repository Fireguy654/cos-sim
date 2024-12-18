module Data.ListZipper
  ( ListZipper (..)
  , listLeft
  , listRight
  , listWrite
  , toList
  , genericMove
  ) where

import           Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

listLeft, listRight :: ListZipper a -> ListZipper a

listLeft (LZ (aLeft:leftAs) a rightAs) = LZ leftAs aLeft (a:rightAs)
listLeft lz                            = lz

listRight (LZ leftAs a (aRight:rightAs)) = LZ (a:leftAs) aRight rightAs
listRight lz                             = lz

listWrite :: a -> ListZipper a -> ListZipper a
listWrite a (LZ leftAs _ rightAs) = LZ leftAs a rightAs

toList :: ListZipper a -> Int -> [a]
toList (LZ leftAs a rightAs) n = reverse (take n leftAs) ++ [a] ++ take n rightAs

genericMove :: (a -> a) -> (a -> a) -> a -> ListZipper a
genericMove lf rf a = LZ (tail $ iterate lf a) a (tail $ iterate rf a)

instance Functor ListZipper where
  fmap f (LZ leftAs a rightAs) = LZ (f <$> leftAs) (f a) (f <$> rightAs)

instance Comonad ListZipper where
  extract (LZ _ a _) = a

  duplicate = genericMove listLeft listRight
