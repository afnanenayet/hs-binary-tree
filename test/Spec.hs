import           Test.QuickCheck
import           Lib
import           Data.List

main :: IO ()
main = quickCheck (withMaxSuccess 10000 prop_inOrderSorted)

prop_inOrderSorted :: [Int] -> Bool
prop_inOrderSorted xs = sorted == traversal
  where
    sorted    = sort xs
    tree      = foldl Lib.insert Empty xs
    traversal = inOrder tree
