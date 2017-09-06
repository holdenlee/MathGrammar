{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XLambdaCase
#-}

module Main where
import System.Environment
import Control.Monad
import Data.Tree
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import qualified Data.Hashable
import Data.Either
import Data.Maybe
import Text.Printf
import Control.Monad.Writer.Lazy
import System.IO

import Text.Read

import Utilities

data Expr = Adj (Int -> Bool)
--          | Adj' ([Int] -> Bool) 
          | SetI [Int]
          | I Int
          | Null 

instance Show Expr where
    show = \case
           Adj f -> "function"
           SetI li -> "list"
           I i -> show i
           Null -> "null"

type Entry = (String, String, [Expr] -> Expr)

listByPriority :: [[Entry]]
listByPriority = [[("even","A",(\_ -> Adj (\x -> x `mod` 2==0))),
                   ("odd","A",(\_ -> Adj (\x -> x `mod` 2==1))),
                   ("greater than N", "A", (\li -> case li!!2 of {I n -> Adj (\x -> x>n)})),
                   ("smaller than N", "A", (\li -> case li!!2 of {I n -> Adj (\x -> x<n)})),
                   ("number", "S", (\_ -> SetI [1..1000]))], 
                  [("in S", "A", (\li -> case (li!!1) of {SetI s -> Adj (`elem` s)}))],
                  [("not A", "A", (\li -> case (li!!1) of {Adj f -> Adj (not . f)}))],
                  [("A and A", "A", (\li -> case (li!!0,li!!2) of {(Adj f, Adj g) -> Adj (\x -> f x && g x)}))],
                  [("A or A", "A", (\li -> case (li!!0,li!!2) of {(Adj f, Adj g) -> Adj (\x -> f x || g x)}))],
                  [("S that is A", "S", (\li -> case (li!!0, li!!3) of {(SetI s, Adj f) -> SetI (filter f s)})),
                   ("A S", "S", (\li -> case (li!!1, li!!0) of {(SetI s, Adj f) -> SetI (filter f s)}))],
                  [("smallest S", "N", (\li -> case (li!!1) of {SetI s -> I (minimum s)})),
                   ("largest S", "N", (\li -> case (li!!1) of {SetI s -> I (maximum s)}))]]
--("number in L", (\li -> li!!2)),

-- "greater than every number in..."

li1 = [1,2,4,5,7,9,10,12]
li2 = [2,4,9,12,15,18]

parseSentence :: String -> Writer [String] Expr --IO Expr 
parseSentence s = do
  let li = words s
  tell [s]
  let (li', vals) = unzip $ map (\x -> case (readMaybe x)::(Maybe Int) of
                                     Just i -> ("N", I i)
                                     Nothing -> case x of
                                                  "li1" -> ("S", SetI li1)
                                                  "li2" -> ("S", SetI li2)
                                                  _ -> (x, Null)) li
  tell [unwords li']
  tell [show vals]
  --putStrLn (unwords li')
  --putStrLn 
  parseSentence' listByPriority li' vals

parseSentence' :: [[Entry]] -> [String] -> [Expr] -> Writer [String] Expr --IO Expr 
parseSentence' liPr li es = do
    let (li', es') = parseSentL liPr li es
    tell [unwords li']
    --putStrLn (unwords li')
    --putStrLn (show es')
    tell [show es']
    if length es' == 1
      then return (es'!!0)
      else parseSentence' liPr li' es'

parseSentL :: [[Entry]] -> [String] -> [Expr]-> ([String], [Expr])
parseSentL liPr li es = 
    case liPr of 
      [] -> (li, es)
      h:rest -> 
          let
              (li', es') = parseSent' h li es
          in if li'==li
             then parseSentL rest li es
             else (li', es')
      

parseSent' :: [Entry] -> [String] -> [Expr]-> ([String], [Expr])
parseSent' p li es = 
          if null li 
          then (li,es)
          else
              let is = filter (\(s,t,f) -> (words s) `elem` (inits li)) p
              in
                  case is of
                    [] ->  
                      let 
                          (li'', es'') = parseSent' p (tail li) (tail es)
                          (li3, es3) = ((head li):li'', (head es):es'')
                      in (li3, es3)
                    (s,t,f):rest -> (t:(drop (length (words s)) li), 
                                      (f $ take (length (words s)) es):(drop (length (words s)) es))

main = do
  line <- getLine
  unless (line == "") $ do
         let (ans, log) = runWriter (parseSentence line)
         putStrLn (unlines (take 100 log))
         hFlush stdout
         main
--smallest number that is in li2 and not in li1
--largest even number that is in li1 and not in li2
--smallest even number that is greater than 6
--(unlines log)
--

{-    if null liPr || null li
    then (li, es)
    else
  -}      
        
