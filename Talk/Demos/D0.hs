import QuickSpec

sig =
    signature {
        maxTermSize = Just 7,
        constants = [
                    constant "zip" (zip :: [Int] -> [Int] -> [(Int, Int)]),
                    constant "length" (length :: [A] -> Int),
                    constant "reverse" (reverse :: [A] -> [A]),
                    constant "++" ((++) :: [Int] -> [Int] -> [Int])
                    ]
    }

main = quickSpec sig
