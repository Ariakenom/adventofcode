import Preface

import qualified Data.Text as T
import qualified Data.Text.IO as T

main = T.interact (tshowLn . solve . parse)

solve :: [Int] -> Int
solve [start, stop] = length [
        ()
        | num <- [start .. stop]
        , or  [a == b | (a,b) <- consecutive $ show $ num]
        , and [a <= b | (a,b) <- consecutive $ show $ num]
    ]

parse = map tread . T.splitOn "-"

consecutive = zip`ap`tail

-- tshow = T.pack . show

tshowLn = T.pack . (++"\n") . show

tread = read . T.unpack
