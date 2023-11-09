import Test.HUnit
import Data.Char 
import Data.List 
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

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
            where
              os = length (filter (==O) ps)
              xs = length (filter (==X) ps)
              ps = concat g  

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              line = all (== p)
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = 
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size * 4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
            where
                beside = foldr1 (zipWith (++))
                bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   "," O ","   "]
showPlayer B = ["   ","   ","   "]
showPlayer X = ["   "," X ","   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

cls :: IO ()
cls = putStr "\ESC[2J"

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g   = putStrLn "Its a draw. \n"
         | otherwise =
              do i <- getNat (prompt p)
                 case move g i p of
                    [] -> do putStrLn "ERROR: Invalid Move"
                             run' g p
                    [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

test1 :: Test
test1 = TestList
  [ valid empty 0 ~?= True
  , valid empty 5 ~?= True
  , valid empty 9 ~?= False
  , valid [[O, X, B], [X, O, B], [O, B, B]] 2 ~?= True
  , valid [[O, X, B], [X, O, B], [O, B, B]] 5 ~?= False
  ]

test2 :: Test
test2 = TestList
  [ wins O [[O,O,O],[B,B,B],[B,B,B]] ~?= True
  , wins O [[O,B,B],[O,B,B],[O,B,B]] ~?= True
  , wins O [[O,B,B],[B,O,B],[B,B,O]] ~?= True
  , wins X [[O,X,X],[O,X,B],[X,O,B]] ~?= True
  , wins X [[O,X,B],[X,X,B],[O,X,B]] ~?= True
  , wins X [[O,X,O],[X,B,O],[O,X,X]] ~?= True
  , wins X [[O,O,X],[X,X,O],[O,X,B]] ~?= False
  , wins O empty ~?= False
  ]

test3 :: Test
test3 = TestList
  [ full [[O,X,O],[O,X,O],[X,O,X]] ~?= True
  , full [[O,X,O],[X,X,O],[O,O,X]] ~?= False
  , full empty ~?= False
  ]

tests = TestList [TestLabel "Function Valid" test1, TestLabel "Function Wins" test2, TestLabel "Function Full" test3]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()