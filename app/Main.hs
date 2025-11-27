module Main (main) where

import MikkTSpace.FFI
import Foreign.Storable (sizeOf)

main :: IO ()
main = do
    putStrLn "MikkTSpace FFI loaded."
    putStrLn $ "Size of SMikkTSpaceContext: " ++ show (sizeOf (undefined :: SMikkTSpaceContext))
