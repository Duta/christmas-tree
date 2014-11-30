{-# LANGUAGE QuasiQuotes #-}
module ChristmasTree where

import Control.Concurrent.Timer (TimerIO, oneShotTimer)
import Control.Concurrent.Suspend.Lifted (Delay)
import Control.Conditional (select)
import Control.Monad (void)
import Data.List (maximumBy, intercalate)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import System.Console.ANSI (clearFromCursorToScreenBeginning, setCursorPosition)
import Text.RawString.QQ (r)

type Tree = String

tree :: Tree
tree = [r|
    *
  . | .
 . \|/ .
. \_|_/ .
 '--|--'
   ###
|]

rawTrees :: [Tree]
rawTrees =
  [[r|
###
|],[r|
 *
###
|],[r|
 *
\|/
###
|],[r|
  *
 \|/
'-|-'
 ###
|],[r|
   *
 . | .
. \|/ .
 '-|-'
  ###
|],[r|
    *
  . | .
 . '|' .
. '.|.' .
 '--|--'
   ###
|],[r|
     *
   . | .
  . '|' .
 . '-|-' .
. '-.|.-' .
 '-._|_.-'
    ###
|],[r|
      *
    . | .
   . '|' .
  . '-|-' .
 . '-.|.-' .
. '-._|_.-' .
 '-.._|_..-'
     ###
|]]

prettyTrees :: [Tree]
prettyTrees = prettify rawTrees

numTrees :: Int
numTrees = length prettyTrees

ensure :: (a -> Bool) -> [a] -> [a]
ensure p = catMaybes . map (select p Just $ const Nothing)

removeAll :: Eq a => a -> [a] -> [a]
removeAll x = ensure (/=x)

onTreeLines :: ([String] -> [String]) -> Tree -> Tree
onTreeLines f = intercalate "\n" . f . lines'

removeCRs :: String -> String
removeCRs = removeAll '\r'

removeEmptys :: Tree -> Tree
removeEmptys = onTreeLines $ removeAll ""

lines' :: String -> [String]
lines' = map removeCRs . lines

maximumOn :: Ord b => (a -> b) -> [a] -> b
maximumOn f = f . maximumBy (comparing f)

width :: Tree -> Int
width = maximumOn length . lines'

maxWidth :: [Tree] -> Int
maxWidth = maximumOn width

padWidth :: Int -> Tree -> Tree
padWidth n = onTreeLines $ map (replicate n ' '++)

padToWidth :: Int -> Tree -> Tree
padToWidth n t = padWidth ((n - width t) `div` 2) t

height :: Tree -> Int
height = length . lines'

maxHeight :: [Tree] -> Int
maxHeight = maximumOn height

padHeight :: Int -> Tree -> Tree
padHeight n = onTreeLines (replicate n ""++)

padToHeight :: Int -> Tree -> Tree
padToHeight n t = padHeight (n - height t) t

prettify :: [Tree] -> [Tree]
prettify ts = map (padToHeight (maxHeight ts') . padToWidth (maxWidth ts')) ts'
 where ts' = map removeEmptys ts

printTrees :: [Tree] -> IO ()
printTrees = mapM_ putStrLn

(!!!) :: [a] -> Int -> a
xs !!! n = xs !! (n `mod` length xs)

runAnimation :: Delay -> IO ()
runAnimation delay = loop 0 >> let x = x in x
 where
  loop :: Int -> IO TimerIO
  loop n = oneShotTimer (action n) delay
  action :: Int -> IO ()
  action n = void $ do
    clearFromCursorToScreenBeginning
    setCursorPosition 0 0
    putStrLn (prettyTrees !!! n)
    loop (n+1)
