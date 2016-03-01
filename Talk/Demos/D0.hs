import QuickSpec

sig =
    signature {
        maxTermSize = Just 7,
        constants = [
                    constant "zip" (zip :: [Int] -> [Int] -> [(Int, Int)]),
                    constant "reverse" (reverse :: [Int] -> [Int]),
                    constant "++" ((++) :: [Int] -> [Int] -> [Int])
                    ]
    }

main = quickSpec sig
