module Dice where

import System.Random ( uniformR, RandomGen )

roll :: RandomGen g => g -> (Int, g)
roll = uniformR (1 :: Int, 3)