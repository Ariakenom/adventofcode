import Preface
import qualified Data.Text as T
import Data.Text.IO as T
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M

main = T.interact (tshowln . solve . parse)

solve mem = exe 0 0 mem [2]

exe :: Integer -> Integer -> M.Map Integer Integer -> [Integer] -> [Integer]
exe rb pc mem is = case op of
        1 -> exe rb (pc+4) (writeMem mode2 (pc+3) (val0 + val1)) is -- add
        2 -> exe rb (pc+4) (writeMem mode2 (pc+3) (val0 * val1)) is -- mul
        3 -> if (not$null$is) then exe rb (pc+2) (writeMem mode0 (pc+1) (head is)) (tail is) -- input
             else error "Wanted more input than available."
        4 -> val0:exe rb (pc+2) mem is -- output
        5 -> exe rb (if val0/=0 then val1 else pc+3) mem is -- jmp true
        6 -> exe rb (if val0==0 then val1 else pc+3) mem is -- jmp false
        7 -> exe rb (pc+4) (writeMem mode2 (pc+3) (viaEnum (val0 <  val1))) is -- less
        8 -> exe rb (pc+4) (writeMem mode2 (pc+3) (viaEnum (val0 == val1))) is -- equal
        9 -> exe (rb+val0) (pc+2) mem is
        99 -> []
        _ -> error $ "invalid opcode: " ++ show op
    where --  01102,2,2,4
        header = mem `readMap` pc
        op     = (header `div` 1e0) `mod` 1e2
        mode0  = (header `div` 1e2) `mod` 1e1
        mode1  = (header `div` 1e3) `mod` 1e1
        mode2  = (header `div` 1e4) `mod` 1e1
        val0   = readMem mode0 (pc+1)
        val1   = readMem mode1 (pc+2)
        addr0  = mem `readMap` (pc+1)
        addr2  = mem `readMap` (pc+3)
        readMem mode a = if
            | mode == 1 -> mem `readMap` a
            | mode == 0 -> mem `readMap` (mem `readMap` a)
            | mode == 2 -> mem `readMap` (rb + (mem `readMap` a))
        writeMem mode a x = if
            | mode == 1 -> error "Writing in immediate mode."
            | mode == 0 -> M.insert (mem `readMap` a) x mem
            | mode == 2 -> M.insert (rb + (mem `readMap` a)) x mem
        readMap mem idx = M.findWithDefault 0 idx mem

parse = M.fromList . zip [0..] . map tread . T.splitOn ","
