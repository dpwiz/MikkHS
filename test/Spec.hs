module Main (main) where

import MikkTSpace.FFI
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc (alloca)
import Control.Monad (forM_)
import Data.IORef
import System.Exit (exitFailure)

-- Basic types
data Vertex = Vertex
    { vx, vy, vz :: Float
    , nx, ny, nz :: Float
    , tx, ty :: Float
    } deriving (Show)

type Face = [Vertex]
type Mesh = [Face]

-- Geometry helper
makeVertex :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float) -> Vertex
makeVertex (px,py,pz) (n_x,n_y,n_z) (u,v) = Vertex px py pz n_x n_y n_z u v

-- Tetrahedron
-- Vertices
v0, v1, v2, v3 :: (Float, Float, Float)
v0 = (1, 1, 1)
v1 = (1, -1, -1)
v2 = (-1, 1, -1)
v3 = (-1, -1, 1)

-- Faces with Flat Normals
-- Normal calculation: (b-a) x (c-a)
cross :: (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float)
cross (ax,ay,az) (bx,by,bz) = (ay*bz - az*by, az*bx - ax*bz, ax*by - ay*bx)

sub :: (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float)
sub (ax,ay,az) (bx,by,bz) = (ax-bx, ay-by, az-bz)

normalize :: (Float,Float,Float) -> (Float,Float,Float)
normalize (x,y,z) = let l = sqrt (x*x + y*y + z*z) in (x/l, y/l, z/l)

faceNormal :: (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float)
faceNormal a b c = normalize (cross (sub b a) (sub c a))

-- Faces (CCW winding)
-- F0: 0, 2, 1
n0, n1, n2, n3 :: (Float, Float, Float)
n0 = faceNormal v0 v2 v1
f0 :: Face
f0 = [makeVertex v0 n0 (0,0), makeVertex v2 n0 (1,0), makeVertex v1 n0 (0,1)]

-- F1: 0, 1, 3
n1 = faceNormal v0 v1 v3
f1 :: Face
f1 = [makeVertex v0 n1 (0,0), makeVertex v1 n1 (1,0), makeVertex v3 n1 (0,1)]

-- F2: 0, 3, 2
n2 = faceNormal v0 v3 v2
f2 :: Face
f2 = [makeVertex v0 n2 (0,0), makeVertex v3 n2 (1,0), makeVertex v2 n2 (0,1)]

-- F3: 1, 2, 3
n3 = faceNormal v1 v2 v3
f3 :: Face
f3 = [makeVertex v1 n3 (0,0), makeVertex v2 n3 (1,0), makeVertex v3 n3 (0,1)]

tetrahedron :: Mesh
tetrahedron = [f0, f1, f2, f3]

-- Callbacks implementation
getNumFaces :: Mesh -> Ptr SMikkTSpaceContext -> IO CInt
getNumFaces mesh _ = return $ fromIntegral (length mesh)

getNumVerticesOfFace :: Mesh -> Ptr SMikkTSpaceContext -> CInt -> IO CInt
getNumVerticesOfFace mesh _ iFace = do
    let face = mesh !! fromIntegral iFace
    return $ fromIntegral (length face)

getPosition :: Mesh -> Ptr SMikkTSpaceContext -> Ptr CFloat -> CInt -> CInt -> IO ()
getPosition mesh _ outPos iFace iVert = do
    let Vertex x y z _ _ _ _ _ = (mesh !! fromIntegral iFace) !! fromIntegral iVert
    pokeElemOff outPos 0 (realToFrac x)
    pokeElemOff outPos 1 (realToFrac y)
    pokeElemOff outPos 2 (realToFrac z)

getNormal :: Mesh -> Ptr SMikkTSpaceContext -> Ptr CFloat -> CInt -> CInt -> IO ()
getNormal mesh _ outNorm iFace iVert = do
    let Vertex _ _ _ n_x n_y n_z _ _ = (mesh !! fromIntegral iFace) !! fromIntegral iVert
    pokeElemOff outNorm 0 (realToFrac n_x)
    pokeElemOff outNorm 1 (realToFrac n_y)
    pokeElemOff outNorm 2 (realToFrac n_z)

getTexCoord :: Mesh -> Ptr SMikkTSpaceContext -> Ptr CFloat -> CInt -> CInt -> IO ()
getTexCoord mesh _ outTex iFace iVert = do
    let Vertex _ _ _ _ _ _ tx ty = (mesh !! fromIntegral iFace) !! fromIntegral iVert
    pokeElemOff outTex 0 (realToFrac tx)
    pokeElemOff outTex 1 (realToFrac ty)

-- Result storage
data TangentSpace = TangentSpace
    { tsFace :: Int
    , tsVert :: Int
    , tsTangent :: (Float, Float, Float)
    , tsBiTangent :: (Float, Float, Float)
    , tsMagS :: Float
    , tsMagT :: Float
    , tsOrient :: Bool
    } deriving (Show)

setTSpace :: IORef [TangentSpace] -> Ptr SMikkTSpaceContext -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> TBool -> CInt -> CInt -> IO ()
setTSpace ref _ tangent bitangent magS magT orient iFace iVert = do
    tx <- peekElemOff tangent 0
    ty <- peekElemOff tangent 1
    tz <- peekElemOff tangent 2
    bx <- peekElemOff bitangent 0
    by <- peekElemOff bitangent 1
    bz <- peekElemOff bitangent 2

    let ts = TangentSpace
            { tsFace = fromIntegral iFace
            , tsVert = fromIntegral iVert
            , tsTangent = (realToFrac tx, realToFrac ty, realToFrac tz)
            , tsBiTangent = (realToFrac bx, realToFrac by, realToFrac bz)
            , tsMagS = realToFrac magS
            , tsMagT = realToFrac magT
            , tsOrient = orient /= 0
            }
    modifyIORef ref (ts :)

main :: IO ()
main = do
    putStrLn "Running MikkTSpace tests..."

    let mesh = tetrahedron
    resultsRef <- newIORef []

    let cbs = MikkTSpaceHSCallbacks
            { cbGetNumFaces          = getNumFaces mesh
            , cbGetNumVerticesOfFace = getNumVerticesOfFace mesh
            , cbGetPosition          = getPosition mesh
            , cbGetNormal            = getNormal mesh
            , cbGetTexCoord          = getTexCoord mesh
            , cbSetTSpaceBasic       = Nothing
            , cbSetTSpace            = Just (setTSpace resultsRef)
            }

    iface <- createMikkTSpaceInterface cbs

    alloca $ \pContext -> do
        alloca $ \pInterface -> do
            poke pInterface iface
            poke pContext (SMikkTSpaceContext pInterface nullPtr)

            putStrLn "Generating tangent space..."
            result <- c_genTangSpaceDefault pContext
            putStrLn $ "Result: " ++ show result

            if result /= 1
                then do
                    putStrLn "Generation failed!"
                    exitFailure
                else putStrLn "Generation successful."

    freeMikkTSpaceInterface iface

    results <- readIORef resultsRef
    putStrLn $ "Generated " ++ show (length results) ++ " tangent spaces."

    -- Verification
    -- Check that we have 12 results (4 faces * 3 verts)
    if length results /= 12
        then do
            putStrLn "Error: Expected 12 results."
            exitFailure
        else putStrLn "Count correct."

    -- Check basic properties
    forM_ results $ \ts -> do
        let (tx, ty, tz) = tsTangent ts
        let len = sqrt (tx*tx + ty*ty + tz*tz)
        if abs (len - 1.0) > 1e-4
            then do
                putStrLn $ "Error: Tangent not normalized: " ++ show ts
                exitFailure
            else return ()

    putStrLn "All tests passed."
