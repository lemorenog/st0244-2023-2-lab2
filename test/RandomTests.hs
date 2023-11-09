import Test.QuickCheck
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

instance Arbitrary Player where
    arbitrary = elements [O, B, X]

prop_next_next :: Player -> Bool
prop_next_next p = next (next p) == p

main :: IO ()
main = do
  quickCheck prop_next_next