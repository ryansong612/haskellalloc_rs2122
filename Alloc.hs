{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Using or on tuple" #-}
module Alloc where

import Data.Maybe
import Data.List

import Types
    ( CFG,
      IdMap,
      Colouring,
      IG,
      Graph,
      Function,
      Block,
      Statement(..),
      Exp(..),
      Id,
      lookUp,
      )
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count a (x:xs)
  = count' a (x:xs) 0
  where
    count' b [] n 
      = n
    count' b (x:xs) !n
      | b == x    = count' b xs (n + 1)
      | otherwise = count' b xs n

count2 :: Eq a => a -> [a] -> Int
count2 x xs
  = length (filter (==x) xs)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (nodes, edges)
  = zip nodes connectednodes
  where
    connectednodes
      = map (\x -> length [c | c@(a,b) <- edges, a == x || b == x]) nodes


neighbours :: Eq a => a -> Graph a -> [a]
neighbours x (n, e)
  = map (\(a,b) -> if a == x then b else a) connectednodes
  where
    connectednodes
      = [c | c@(a,b) <- e, a == x || b == x]


removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n g@([], es) = g
removeNode n (ns, es)
  = (ns', es')
  where
    ns' = filter (/=n) ns
    es' = filter (\(x,y) -> (x /= n) && (y /= n)) es

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph x ([], _) = []
colourGraph x g@(ns, es)
  = (n, nc') : cs
    where
      order = degrees g
      n     = minimum [a | (a,b) <- order, b == snd (minimum order)]
      g'    = removeNode n g
      cs    = colourGraph x g'
      ns    = neighbours n g
      cols  = [col | (l, col) <- cs, l `elem` ns, col /= 0]
      mex is
        = mex' ((sort . nub) is) 1
        where
          mex' [] n 
            = n
          mex' (i:is) n
            | i == n    = mex' is (n + 1)
            | otherwise = n
      nc  = mex cols
      nc' = if nc > x then 0 else nc
  
------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap m
  = ("return", "return") : reverse (buildIdMap' m [])
  where
    buildIdMap' [] s 
      = s
    buildIdMap' (c@(n,b):cs) s
      | b == 0    = buildIdMap' cs ((n,n):s)
      | otherwise = buildIdMap' cs ((n,r):s)
      where
        r = "R" ++ show b


buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments vs idm
  = zipWith Assign rs vars
  where
    rs   = map (`lookUp` idm) vs
    vars = map Var vs

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Var x) map 
  = Var (lookUp x map)
renameExp (Const x) _
  = Const x
renameExp (Apply op e1 e2) map 
  = Apply op e1' e2'
  where
    e1' = renameExp e1 map
    e2' = renameExp e2 map

renameStatement :: Statement -> IdMap -> [Statement]
renameStatement (Assign r (Var r')) m
  | (lookUp r m) == (lookUp r' m) = []
renameStatement (Assign i e) m 
  = [Assign (lookUp i m) (renameExp e m)]
renameStatement (If e b1 b2) m 
  = [If (renameExp e m) (renameBlock b1 m) (renameBlock b2 m)]
renameStatement (While e b) m
  = [While (renameExp e m) (renameBlock b m)]


renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock b m = concatMap (`renameStatement` m) b

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG ids
  = (vars, liveVars)
  where
    vars = nub (concat ids)
    liveVars 
      = nub (concatMap (\v -> [ (min v1 v2, max v1 v2) | v1 <- v, v2 <- v, v1 /= v2]) ids)

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined