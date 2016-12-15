import Test.Tasty

import DeltaCRDTSpec
import SemiLatticeActionSpec

main :: IO ()
main = defaultMain $ testGroup "all CDRT'"
    [ setIsDCRDT'
    , orderedIntIsDCRDT'
    , setIsSemiLatticeAction
    ]
