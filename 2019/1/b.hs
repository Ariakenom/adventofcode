import Data.Function

main = interact (lines%map read%map go%sum%showLn)

go m = iterate mass2fuel m & drop 1 & map (max 0) & takeWhile (/=0) & sum

mass2fuel m = m `div` 3 - 2

showLn = show % (++"\n")

(%) = flip (.)
