import Prelude hiding (insert)
import QuickSpec hiding (insert)
import Test.QuickCheck
import SetList

sig =
    signature {
        maxTermSize = Just 7,
        constants = [
                        constant "True" True,
                        constant "member" (member :: Integer -> [Integer] -> Bool),
                        constant "insert" (insert :: Integer -> [Integer] -> [Integer]),
                        constant "union" (union :: [Integer] -> [Integer] -> [Integer])
                    ]
    }

main = quickSpec sig
