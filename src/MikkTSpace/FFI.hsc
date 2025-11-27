{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module MikkTSpace.FFI
    ( -- * Types
      TBool
    , SMikkTSpaceContext(..)
    , SMikkTSpaceInterface(..)

      -- * Callback types
    , GetNumFacesCallback
    , GetNumVerticesOfFaceCallback
    , GetPositionCallback
    , GetNormalCallback
    , GetTexCoordCallback
    , SetTSpaceBasicCallback
    , SetTSpaceCallback

      -- * C Functions
    , c_genTangSpaceDefault
    , c_genTangSpace

      -- * Callback wrappers
    , mkGetNumFaces
    , mkGetNumVerticesOfFace
    , mkGetPosition
    , mkGetNormal
    , mkGetTexCoord
    , mkSetTSpaceBasic
    , mkSetTSpace
    ) where

import Foreign
import Foreign.C

#include "mikktspace.h"

-- | tbool is typedef int tbool;
type TBool = CInt

-- | struct SMikkTSpaceContext
data SMikkTSpaceContext = SMikkTSpaceContext
    { m_pInterface :: Ptr SMikkTSpaceInterface
    , m_pUserData  :: Ptr ()
    } deriving (Show, Eq)

instance Storable SMikkTSpaceContext where
    sizeOf _    = #{size SMikkTSpaceContext}
    alignment _ = #{alignment SMikkTSpaceContext}
    peek ptr = do
        pInterface <- #{peek SMikkTSpaceContext, m_pInterface} ptr
        pUserData  <- #{peek SMikkTSpaceContext, m_pUserData} ptr
        return $ SMikkTSpaceContext pInterface pUserData
    poke ptr (SMikkTSpaceContext pInterface pUserData) = do
        #{poke SMikkTSpaceContext, m_pInterface} ptr pInterface
        #{poke SMikkTSpaceContext, m_pUserData} ptr pUserData

-- | Callbacks
type GetNumFacesCallback = Ptr SMikkTSpaceContext -> IO CInt
type GetNumVerticesOfFaceCallback = Ptr SMikkTSpaceContext -> CInt -> IO CInt
type GetPositionCallback = Ptr SMikkTSpaceContext -> Ptr CFloat -> CInt -> CInt -> IO ()
type GetNormalCallback = Ptr SMikkTSpaceContext -> Ptr CFloat -> CInt -> CInt -> IO ()
type GetTexCoordCallback = Ptr SMikkTSpaceContext -> Ptr CFloat -> CInt -> CInt -> IO ()
type SetTSpaceBasicCallback = Ptr SMikkTSpaceContext -> Ptr CFloat -> CFloat -> CInt -> CInt -> IO ()
type SetTSpaceCallback = Ptr SMikkTSpaceContext -> Ptr CFloat -> Ptr CFloat -> CFloat -> CFloat -> TBool -> CInt -> CInt -> IO ()

-- | struct SMikkTSpaceInterface
data SMikkTSpaceInterface = SMikkTSpaceInterface
    { m_getNumFaces          :: FunPtr GetNumFacesCallback
    , m_getNumVerticesOfFace :: FunPtr GetNumVerticesOfFaceCallback
    , m_getPosition          :: FunPtr GetPositionCallback
    , m_getNormal            :: FunPtr GetNormalCallback
    , m_getTexCoord          :: FunPtr GetTexCoordCallback
    , m_setTSpaceBasic       :: FunPtr SetTSpaceBasicCallback
    , m_setTSpace            :: FunPtr SetTSpaceCallback
    } deriving (Show, Eq)

instance Storable SMikkTSpaceInterface where
    sizeOf _    = #{size SMikkTSpaceInterface}
    alignment _ = #{alignment SMikkTSpaceInterface}
    peek ptr = do
        getNumFaces          <- #{peek SMikkTSpaceInterface, m_getNumFaces} ptr
        getNumVerticesOfFace <- #{peek SMikkTSpaceInterface, m_getNumVerticesOfFace} ptr
        getPosition          <- #{peek SMikkTSpaceInterface, m_getPosition} ptr
        getNormal            <- #{peek SMikkTSpaceInterface, m_getNormal} ptr
        getTexCoord          <- #{peek SMikkTSpaceInterface, m_getTexCoord} ptr
        setTSpaceBasic       <- #{peek SMikkTSpaceInterface, m_setTSpaceBasic} ptr
        setTSpace            <- #{peek SMikkTSpaceInterface, m_setTSpace} ptr
        return $ SMikkTSpaceInterface getNumFaces getNumVerticesOfFace getPosition getNormal getTexCoord setTSpaceBasic setTSpace
    poke ptr val = do
        #{poke SMikkTSpaceInterface, m_getNumFaces} ptr (m_getNumFaces val)
        #{poke SMikkTSpaceInterface, m_getNumVerticesOfFace} ptr (m_getNumVerticesOfFace val)
        #{poke SMikkTSpaceInterface, m_getPosition} ptr (m_getPosition val)
        #{poke SMikkTSpaceInterface, m_getNormal} ptr (m_getNormal val)
        #{poke SMikkTSpaceInterface, m_getTexCoord} ptr (m_getTexCoord val)
        #{poke SMikkTSpaceInterface, m_setTSpaceBasic} ptr (m_setTSpaceBasic val)
        #{poke SMikkTSpaceInterface, m_setTSpace} ptr (m_setTSpace val)

-- | Functions
foreign import ccall "genTangSpaceDefault"
    c_genTangSpaceDefault :: Ptr SMikkTSpaceContext -> IO TBool

foreign import ccall "genTangSpace"
    c_genTangSpace :: Ptr SMikkTSpaceContext -> CFloat -> IO TBool

-- | Wrappers to create FunPtrs
foreign import ccall "wrapper"
    mkGetNumFaces :: GetNumFacesCallback -> IO (FunPtr GetNumFacesCallback)

foreign import ccall "wrapper"
    mkGetNumVerticesOfFace :: GetNumVerticesOfFaceCallback -> IO (FunPtr GetNumVerticesOfFaceCallback)

foreign import ccall "wrapper"
    mkGetPosition :: GetPositionCallback -> IO (FunPtr GetPositionCallback)

foreign import ccall "wrapper"
    mkGetNormal :: GetNormalCallback -> IO (FunPtr GetNormalCallback)

foreign import ccall "wrapper"
    mkGetTexCoord :: GetTexCoordCallback -> IO (FunPtr GetTexCoordCallback)

foreign import ccall "wrapper"
    mkSetTSpaceBasic :: SetTSpaceBasicCallback -> IO (FunPtr SetTSpaceBasicCallback)

foreign import ccall "wrapper"
    mkSetTSpace :: SetTSpaceCallback -> IO (FunPtr SetTSpaceCallback)
