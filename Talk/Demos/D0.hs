import QuickSpec

-- QuickSpec signature
sig =
    signature {
        maxTermSize = Just 7,
        constants = [
                    constant "zip"     (zip :: [A] -> [B] -> [(A, B)]),
                    constant "reverse" (reverse :: [A] -> [A]),
                    constant "++"      ((++) :: [A] -> [A] -> [A])
                    ]
    }

main = quickSpec sig
