{-# LANGUAGE TemplateHaskell #-}
import TemplateDerivingFunctions

different = (/=)
$(mk_When_Function [| different :: Int -> Int -> Bool |] [| (1, 0) |])
