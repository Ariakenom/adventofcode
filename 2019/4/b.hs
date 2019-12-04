import Preface

import qualified Data.Text as T
import qualified Data.Text.IO as T

main = T.interact (tshowLn . solve . parse)

solve :: [Int] -> Int
solve [start, stop] = length [
        ()
        | num <- [start .. stop]
        , test $ digits num
        , and [a <= b | (a,b) <- consecutive $ digits $ num]
    ]

parse = map tread . T.splitOn "-"

test [] = False
test [_] = False
test (x:y:xs) | x /= y = test (y:xs)
-- x == y below
test (x:y:z:xs) | y == z = test (dropWhile (==z) xs)
test _ = True

digits :: Int -> [Int]
digits = map (\c -> read [c]) . show

consecutive = zip`ap`tail


tshowLn = T.pack . (++"\n") . show

tread = read . T.unpack
