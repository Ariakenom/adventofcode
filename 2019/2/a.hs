main = interact (showLn . run 0 . patch . parse)

run :: Int -> [Int] -> Int
run i xs = case op of
        1 -> run i' (set j (a+b) xs)
        2 -> run i' (set j (a*b) xs)
        99 -> xs !! 0
    where
        op = xs !! i
        a  = xs !! (xs !! (i+1))
        b  = xs !! (xs !! (i+2))
        j  = xs !! (i+3)
        i' = i+4

patch = set 2 2 . set 1 12

set i x xs = take i xs ++ [x] ++ drop (i+1) xs

parse :: [Char] -> [Int]
parse "" = []
parse s =
    let
        x  = read (takeWhile (/=',') s)
        xs = drop 1 (dropWhile (/=',') s)
    in  x: parse xs

showLn = (++"\n") . show
