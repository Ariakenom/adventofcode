import qualified Data.Map as M
import Data.Function


main = interact (showLn . solve . map (move2pos ((0,0),0)) . parse)

solve [pos0, pos1] =
    let
        crosses = M.toList (M.intersectionWith (+) (M.fromList pos0) (M.fromList pos1))
        dists = map (\((x,y), s) -> s) crosses
    in
        minimum dists

move2pos :: ((Int, Int),Int) -> [String] -> [((Int, Int),Int)]
move2pos _ [] = []
move2pos ((x,y),s) (('U': nt): ms) = let n = read nt; pos = ((x,y+n),s+n) in [((x,y+i),s+i)| i <- [1..n]] ++ move2pos pos ms
move2pos ((x,y),s) (('D': nt): ms) = let n = read nt; pos = ((x,y-n),s+n) in [((x,y-i),s+i)| i <- [1..n]] ++ move2pos pos ms
move2pos ((x,y),s) (('R': nt): ms) = let n = read nt; pos = ((x+n,y),s+n) in [((x+i,y),s+i)| i <- [1..n]] ++ move2pos pos ms
move2pos ((x,y),s) (('L': nt): ms) = let n = read nt; pos = ((x-n,y),s+n) in [((x-i,y),s+i)| i <- [1..n]] ++ move2pos pos ms

-- move2pos = drop 1 . scanl (&) (0,0) . map f
--     where
--         f ('U': n) = \(x,y) -> (x,          y + n)
--         f ('D': n) = \(x,y) -> (x,          y - n)
--         f ('R': n) = \(x,y) -> (x + n, y)
--         f ('L': n) = \(x,y) -> (x - n, y)

parse =
      map words
    . lines
    . map (\c -> if c == ',' then ' ' else c)


showLn = (++"\n") . show
