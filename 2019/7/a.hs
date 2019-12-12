import Preface
import qualified Data.Text as T
import Data.Text.IO as T
import qualified Data.Sequence as S


main = T.interact (tShowLn . search . parse)

search mem =
    maximum [
        head (settings mem xs)
        | xs <- permutations [0..4]
    ]

settings mem [a,b,c,d,e] =
    let
        r0 = exe 0 mem [a,0]
        r1 = exe 0 mem (b:r0)
        r2 = exe 0 mem (c:r1)
        r3 = exe 0 mem (d:r2)
        r4 = exe 0 mem (e:r3)
    in r4

exe :: Int -> S.Seq Int -> [Int] -> [Int]
exe pc mem is = case op of
        1 -> exe (pc+4) (S.update addr2 (val0 + val1) mem) is -- add
        2 -> exe (pc+4) (S.update addr2 (val0 * val1) mem) is -- mul
        3 -> exe (pc+2) (S.update addr0 (head is) mem) (tail is) -- input
        4 -> val0:exe (pc+2) mem is -- output
        5 -> exe (if val0/=0 then val1 else pc+3) mem is -- jmp true
        6 -> exe (if val0==0 then val1 else pc+3) mem is -- jmp false
        7 -> exe (pc+4) (S.update addr2 (viaEnum (val0 < val1)) mem) is -- less
        8 -> exe (pc+4) (S.update addr2 (viaEnum (val0 == val1)) mem) is -- equal
        99 -> []
        _ -> error $ "invalid opcode: " ++ show op
    where
        header = mem `S.index` pc
        op     = (header `div` 1e0) `mod` 1e2
        mode0  = (header `div` 1e2) `mod` 1e1
        mode1  = (header `div` 1e3) `mod` 1e1
        mode2  = (header `div` 1e4) `mod` 1e1
        val0   = mem `S.index` (if mode0==1 then pc+1 else mem `S.index` (pc+1))
        val1   = mem `S.index` (if mode1==1 then pc+2 else mem `S.index` (pc+2))
        addr0  = mem `S.index` (pc+1)
        addr2  = mem `S.index` (pc+3)

parse = S.fromList . map tRead . T.splitOn ","

tRead = read . T.unpack

showLn = (++"\n") . show

tShowLn = T.pack . showLn
