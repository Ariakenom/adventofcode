import Preface
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import qualified Data.Set as Set

main = T.interact (tshowln . solve . parse)

solve xs =
    let
        neighbors' = foldl' (\m (a,b) -> Map.insertWith (++) a [b] m) Map.empty xs
        neighbors = foldl' (\m (a,b) -> Map.insertWith (++) b [a] m) neighbors' xs
        go seen n "SAN" = Just n
        go seen n x =
            let
                seen' = Set.insert x seen
                visit = filter (`Set.notMember`seen)
                    $ Map.findWithDefault [] x neighbors
            in first $ map (go seen' (n+1)) visit
    in fmap (subtract 2) (go Set.empty 0 "YOU")

first xs = case filter isJust xs of
    [] -> Nothing
    x:_ -> x

parse =
      map (\[a,b]->(a,b))
    . map (T.split (')'==))
    . T.lines