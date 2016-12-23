import           Test.Tasty

import           DeltaCRDTSpec
import           LesserActionSpec
import           SemiLatticeActionSpec

main :: IO ()
main = defaultMain $ testGroup "all"
    [ setIsDCRDT'
    , orderedIntIsDCRDT'
    , allLesserAction
    , allSemiLatticeAction
    ]
