module Main (main) where

import MikkTSpace.FFI
import Foreign.Storable (sizeOf)
import Foreign.C.Types (CInt, CFloat)
import Foreign.Ptr (Ptr)

-- Dummy callbacks
dummyGetNumFaces :: Ptr SMikkTSpaceContext -> IO CInt
dummyGetNumFaces _ = return 0

dummyGetNumVerticesOfFace :: Ptr SMikkTSpaceContext -> CInt -> IO CInt
dummyGetNumVerticesOfFace _ _ = return 3

dummyGetPosition :: Ptr SMikkTSpaceContext -> Ptr CFloat -> CInt -> CInt -> IO ()
dummyGetPosition _ _ _ _ = return ()

dummyGetNormal :: Ptr SMikkTSpaceContext -> Ptr CFloat -> CInt -> CInt -> IO ()
dummyGetNormal _ _ _ _ = return ()

dummyGetTexCoord :: Ptr SMikkTSpaceContext -> Ptr CFloat -> CInt -> CInt -> IO ()
dummyGetTexCoord _ _ _ _ = return ()

main :: IO ()
main = do
    putStrLn "MikkTSpace FFI loaded."
    putStrLn $ "Size of SMikkTSpaceContext: " ++ show (sizeOf (undefined :: SMikkTSpaceContext))

    let cbs = MikkTSpaceHSCallbacks
            { cbGetNumFaces          = dummyGetNumFaces
            , cbGetNumVerticesOfFace = dummyGetNumVerticesOfFace
            , cbGetPosition          = dummyGetPosition
            , cbGetNormal            = dummyGetNormal
            , cbGetTexCoord          = dummyGetTexCoord
            , cbSetTSpaceBasic       = Nothing
            , cbSetTSpace            = Nothing
            }

    putStrLn "Creating interface..."
    iface <- createMikkTSpaceInterface cbs
    putStrLn "Interface created."

    -- Here we would use it...

    putStrLn "Freeing interface..."
    freeMikkTSpaceInterface iface
    putStrLn "Interface freed."
