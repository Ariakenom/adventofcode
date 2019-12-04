import qualified Data.Text as T
import Data.Text.IO as T
import qualified Data.Sequence as S


main = T.interact (tShowLn . search . parse)

search xs = [
        100*noun + verb
        | noun <- [0..99]
        , verb <- [0..99]
        , run 0 (patch noun verb xs) == 19690720
    ]

run :: Int -> S.Seq Int -> Int
run i xs = case op of
        1 -> run i' (S.update j (a+b) xs)
        2 -> run i' (S.update j (a*b) xs)
        99 -> xs `S.index` 0
    where
        op = xs `S.index` i
        a  = xs `S.index` (xs `S.index` (i+1))
        b  = xs `S.index` (xs `S.index` (i+2))
        j  = xs `S.index` (i+3)
        i' = i+4

patch noun verb = S.update 2 verb . S.update 1 noun

parse = S.fromList . map tRead . T.splitOn ","

tRead = read . T.unpack

showLn = (++"\n") . show

tShowLn = T.pack . showLn
