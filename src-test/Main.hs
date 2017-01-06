import           Test.Tasty

import           LesserActionSpec
import           SemiLatticeActionSpec

main :: IO ()
main = defaultMain $ testGroup "all"
    [ allLesserAction
    , allSemiLatticeAction
    ]
