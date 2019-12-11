import qualified Data.Text as T
import Data.Text.IO as T
import qualified Data.Sequence as S


main = interact (showLn . run 0 . parse)

run :: Int -> S.Seq Int -> [Int] -> [Int]
run i xs = case op of
        1 -> run (i+4) (S.update addr2 (val0 + val1) xs) -- add
        2 -> run (i+4) (S.update addr2 (val0 * val1) xs) -- mul
        3 -> run (i+2) (S.update addr2 1 xs) -- input
        4 -> val0:run (i+2) xs-- output
        99 -> xs `S.index` 0
    where
        header = xs `S.index` i
        op    = (header / 1e0) `mod` 1e2
        mode0 = (header / 1e2) `mod` 1e1
        mode1 = (header / 1e3) `mod` 1e1
        mode2 = (header / 1e4) `mod` 1e1 
        val0  = xs `S.index` if mode0==1 then i+1 else xs `S.index` (i+1)
        val1  = xs `S.index` if mode0==1 then i+2 else xs `S.index` (i+2)
        addr2  = xs `S.index` (i+3)

parse = S.fromList . map tRead . T.splitOn ","

tRead = read . T.unpack

showLn = (++"\n") . show

tShowLn = T.pack . showLn
