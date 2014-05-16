{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Bridging to and from @NSData@
module ObjectiveHaskellMini.NSData (
        fromNSData, toNSData
    ) where

import Control.Applicative
import Data.ByteString.Lazy as ByteString
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import ObjectiveHaskellMini.ObjC
import Control.Monad

-- NSData methods
foreign import ccall safe "dynamic" dataWithBytes_dyn
   :: FunPtr (UnsafeId -> Sel -> Ptr () -> NSUInteger -> IO UnsafeId)
     -> UnsafeId -> Sel -> Ptr () -> NSUInteger -> IO UnsafeId
dataWithBytes :: Ptr () -> NSUInteger -> Class -> IO Id
dataWithBytes bytesPtr len self = do 
    _cmd <- selector "dataWithBytes:length:";
    (withUnsafeId self (\uSelf -> (dataWithBytes_dyn (castFunPtr p_objc_msgSend)) uSelf _cmd bytesPtr len))
        >>= retainedId

foreign import ccall safe "dynamic" objc_length_dyn
   :: FunPtr (UnsafeId -> Sel -> IO NSUInteger)
     -> UnsafeId -> Sel -> IO NSUInteger
objc_length :: Id -> IO NSUInteger
objc_length self = do
    _cmd <- selector "length";
    withUnsafeId self $ \uSelf -> 
        (objc_length_dyn (castFunPtr p_objc_msgSend)) uSelf _cmd

foreign import ccall safe "dynamic" bytes_dyn
   :: FunPtr (UnsafeId -> Sel -> IO (Ptr ()))
     -> UnsafeId -> Sel -> IO (Ptr ())
bytes :: Id -> IO (Ptr ())
bytes self = do
    _cmd <- selector "bytes";
    withUnsafeId self $ \uSelf -> 
        (bytes_dyn (castFunPtr p_objc_msgSend)) uSelf _cmd

-- | Converts an @NSData@ object into a lazy 'ByteString'.
-- | Note that this /does not/ reuse the internal storage of the @NSData@ object, and so may not be suitable for large blobs.
fromNSData :: Id -> IO ByteString
fromNSData dat = do
    sz <- objc_length dat
    ptr <- bytes dat

    pack <$> peekArray (fromIntegral sz) (castPtr ptr)

-- | Converts a lazy 'ByteString' into an immutable @NSData@ object.
-- | Note that this /does not/ reuse the internal storage of the 'ByteString', and so may not be suitable for large blobs.
toNSData :: ByteString -> IO Id
toNSData str =
    withArray (unpack str) $ \ptr ->
        getClass "NSData" >>= dataWithBytes (castPtr ptr) (fromIntegral $ ByteString.length str)

instance Bridged ByteString where
    toObjC = toNSData
    fromObjC = fromNSData

fromNSDataObjC :: Id -> IO (StablePtr ByteString)
fromNSDataObjC obj = fromNSData obj >>= newStablePtr

toNSDataObjC :: StablePtr ByteString -> IO Id
toNSDataObjC ptr = deRefStablePtr ptr >>= toNSData

foreign export ccall "OHHaskellPtrFromNSData" _hs_OHHaskellPtrFromNSData :: UnsafeId -> IO (StablePtr ByteString)
_hs_OHHaskellPtrFromNSData :: UnsafeId -> IO (StablePtr ByteString)
_hs_OHHaskellPtrFromNSData = fromNSDataObjC <=< retainedId

foreign export ccall "OHNSDataFromHaskellPtr" _hs_OHNSDataFromHaskellPtr :: StablePtr ByteString -> IO UnsafeId
_hs_OHNSDataFromHaskellPtr :: StablePtr ByteString -> IO UnsafeId
_hs_OHNSDataFromHaskellPtr = autorelease <=< toNSDataObjC

