import Preface
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map

main = T.interact (tshowln . solve . parse)

solve xs =
    let
        neighbors = foldl' (\m (a,b) -> Map.insertWith (++) a [b] m) Map.empty xs
        go n x = n + sum (map (go (n+1)) (Map.findWithDefault [] x neighbors))
    in go 0 "COM"


parse =
      map (\[a,b]->(a,b))
    . map (T.split (')'==))
    . T.lines
