import Preface
import qualified Data.Text as T
import Data.Text.IO as T
import qualified Data.Sequence as S


main = T.interact (tShowLn . run 0 . parse)

run :: Int -> S.Seq Int -> [Int]
run i xs = case op of
        1 -> run (i+4) (S.update addr2 (val0 + val1) xs) -- add
        2 -> run (i+4) (S.update addr2 (val0 * val1) xs) -- mul
        3 -> run (i+2) (S.update addr0 5 xs) -- input
        4 -> val0:run (i+2) xs-- output
        5 -> run (if val0/=0 then val1 else i+3) xs -- jmp true
        6 -> run (if val0==0 then val1 else i+3) xs -- jmp false
        7 -> run (i+4) (S.update addr2 (viaEnum (val0 < val1)) xs) -- less
        8 -> run (i+4) (S.update addr2 (viaEnum (val0 == val1)) xs) -- equal
        99 -> []
        _ -> error $ "invalid opcode: " ++ show op
    where
        header = xs `S.index` i
        op    = (header `div` 1e0) `mod` 1e2
        mode0 = (header `div` 1e2) `mod` 1e1
        mode1 = (header `div` 1e3) `mod` 1e1
        mode2 = (header `div` 1e4) `mod` 1e1
        val0  = xs `S.index` (if mode0==1 then i+1 else xs `S.index` (i+1))
        val1  = xs `S.index` (if mode1==1 then i+2 else xs `S.index` (i+2))
        addr0  = xs `S.index` (i+1)
        addr2  = xs `S.index` (i+3)

parse = S.fromList . map tRead . T.splitOn ","

tRead = read . T.unpack

showLn = (++"\n") . show

tShowLn = T.pack . showLn
