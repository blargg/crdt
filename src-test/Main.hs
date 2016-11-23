import Test.Tasty

import DeltaCRDTSpec

main :: IO ()
main = defaultMain $ testGroup "all CDRT'"
    [ setIsDCRDT'
    , orderedIntIsDCRDT'
    ]
