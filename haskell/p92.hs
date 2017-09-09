------------------------------------
-- |
-- Project euler problem 92 solution
-- author Tarek
--
------------------------------------

import Control.Monad.State
import qualified Data.IntMap as M

digits :: Int -> [Int]
digits x =
  let s = show x
  in map (\c -> fromEnum c - fromEnum '0') s

square x = x * x

step :: Int -> Int
step = sum . (map square) . digits

terminator :: Int -> Int
terminator n
  | n' == 1 = 1
  | n' == 89 = 89
  | otherwise = terminator n'
  where n' = step n

terminator' :: Int -> State (M.IntMap Int) Int
terminator' n = do
  m <- get
  if M.member n m
    then return (m M.! n)
    else do t <- terminator (step n)
            modify (M.insert n t)
            return t


terminator :: Int -> State (M.IntMap Int) Int
terminator n =
  if n == 1 || n == 89
  then n
  else terminator' n

terminators :: [Int] -> State (M.IntMap Int) [Int]
terminators = mapM terminator

main :: IO ()
main = do
  let l = evalState (terminator [1..1000000]) M.empty
      res = (length . filter (== 89)) l
  putStrLn res
