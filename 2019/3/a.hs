import qualified Data.Set as S
import Data.Function


main = interact (showLn . solve . map (move2pos (0,0)) . parse)

solve [pos0, pos1] =
    let
        crosses = S.toList (S.fromList pos0 `S.intersection` S.fromList pos1)
        dists = map (\(x,y) -> abs x+abs y) crosses
    in
        minimum dists

move2pos :: (Int, Int) -> [String] -> [(Int,Int)]
move2pos (x,y) [] = []
move2pos (x,y) (('U': n): ms) = let pos = (x,y+read n) in [(x,y+i)| i <- [1..read n]] ++ move2pos pos ms
move2pos (x,y) (('D': n): ms) = let pos = (x,y-read n) in [(x,y-i)| i <- [1..read n]] ++ move2pos pos ms
move2pos (x,y) (('R': n): ms) = let pos = (x+read n,y) in [(x+i,y)| i <- [1..read n]] ++ move2pos pos ms
move2pos (x,y) (('L': n): ms) = let pos = (x-read n,y) in [(x-i,y)| i <- [1..read n]] ++ move2pos pos ms

-- move2pos = drop 1 . scanl (&) (0,0) . map f
--     where
--         f ('U': n) = \(x,y) -> (x,          y + read n)
--         f ('D': n) = \(x,y) -> (x,          y - read n)
--         f ('R': n) = \(x,y) -> (x + read n, y)
--         f ('L': n) = \(x,y) -> (x - read n, y)


parse =
      map words
    . lines
    . map (\c -> if c == ',' then ' ' else c)


showLn = (++"\n") . show
