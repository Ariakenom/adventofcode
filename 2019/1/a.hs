main = interact (lines%map read%map mass2fuel%sum%showLn)

mass2fuel m = m `div` 3 - 2

showLn = show % (++"\n")

(%) = flip (.)
