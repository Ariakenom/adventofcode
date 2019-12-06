module Preface (
    module Preface,
    satisfy, anySingle, sepBy
) where

import System.Environment

import Text.Printf

import Text.Megaparsec (setInput, anySingle, satisfy, parse, Parsec, eof, sepBy)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (errorBundlePretty)

import Control.DeepSeq as Preface (
    NFData(..)
  , ($!!)
  , deepseq
  , force
  )

import Safe as Preface

import Data.Foldable (toList)

import Control.Applicative as Preface
import Control.Monad as Preface
import Control.Exception as Preface
import Control.Concurrent as Preface
import Control.Concurrent.Async as Preface

import Data.Void
import Data.Int as Preface
import Data.Bits as Preface
import Data.Word as Preface
import Data.Bool as Preface
import Data.Char as Preface (Char)
import Data.Maybe as Preface
import Data.Either as Preface
import Data.Complex as Preface
import Data.Function as Preface
import Data.List as Preface
import Data.Tuple as Preface
import Data.Map as Preface (Map)
import Data.Set as Preface (Set)
import Data.Sequence as Preface (Seq)
import Data.IntMap as Preface (IntMap)
import Data.IntSet as Preface (IntSet)


import qualified Data.Set as Set
import qualified Data.Map as Map


-- glguy


-- | Get the input for the given day.
--
-- If a filename is provided in the command line that will be used as the
-- input file.
--
-- If the filename is @-@ the stdin will be used as the input file.
--
-- Otherwise the input text file corresponding to the day number will be used.
getRawInput :: Int {- ^ day number -} -> IO String
getRawInput i =
  do args <- getArgs
     case args of
       []    -> readFile (printf "inputs/input%02d.txt" i)
       "-":_ -> getContents
       fn:_  -> readFile fn

inputFileName :: Int -> FilePath
inputFileName = printf "inputs/input%02d.txt"

getInputLines :: Int -> IO [String]
getInputLines i = lines <$> getRawInput i

type Parser = Parsec Void String

getParsedInput :: Int -> Parser a -> IO a
getParsedInput i p =
  do input <- getRawInput i
     case parse p "input" input of
       Left e -> fail (errorBundlePretty e)
       Right a -> return a

-- | Run a parser with 'parseLines' on the input file.
getParsedLines :: Int -> Parser a -> IO [a]
getParsedLines i p =
  do input <- getRawInput i
     either fail return (parseLines p input)

-- | Run a parser on each line of the input file. Each line will be parsed
-- in isolation. The parser must consume the whole line.
--
-- >>> parseLines (many anySingle) "12\n34\n"
-- Right ["12","34"]
-- >>> parseLines number "12\n34\n"
-- Right [12,34]
parseLines :: Parser a -> String -> Either String [a]
parseLines p input =
  case parse (traverse parse1 (lines input)) "input" input of
    Left  e -> Left (errorBundlePretty e)
    Right a -> Right a
  where
    parse1 x = setInput x *> p <* eof <* setInput "\n" <* newline

-- | Count the number of elements in a foldable value that satisfy a predicate.
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\acc x -> if p x then acc+1 else acc) 0


-- | Return true when the whole list is comprised of equal elements.
--
-- >>> same [1,1,1]
-- True
-- >>> same []
-- True
-- >>> same [1]
-- True
-- >>> same [1,1,2]
-- False
same :: Foldable t => Eq a => t a -> Bool
same xs = all (head (toList xs) ==) xs

-- | Returns a list of ways to select an element from a list without
-- replacement.
--
-- >>> pickOne []
-- []
-- >>> pickOne [1]
-- [(1,[])]
-- >>> pickOne [1,2,3]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pickOne :: [a] -> [(a, [a])]
pickOne xs = [ (x, l++r) | (l,x:r) <- zip (inits xs) (tails xs) ]

-- | Parse a signed integral number
number :: Integral a => Parser a
number = signed (return ()) decimal

-- | Implementation of 'nub' that uses 'Ord' for efficiency.
ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise         = x : go (Set.insert x seen) xs


-- | Compute the minimum element of a list or return Nothing if it is empty.
--
-- >>> minimumMaybe []
-- Nothing
-- >>> minimumMaybe [2,1,3]
-- Just 1
minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $! minimum xs

-- | Compute the number of occurrences of the elements in a given list.
--
-- >>> cardinality "bababc"
-- fromList [('a',2),('b',3),('c',1)]
cardinality :: Ord a => [a] -> Map.Map a Int
cardinality xs = Map.fromListWith (+) [ (x,1) | x <- xs ]
-- glguy

csv = words . map (\c -> if c==',' then ' ' else c)
uncsv = concat . intersperse ","

