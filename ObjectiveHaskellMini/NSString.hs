{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Bridging to and from @NSString@
module ObjectiveHaskellMini.NSString (
        fromNSString, toNSString
    ) where

import Control.Monad
import Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy.UTF8
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import ObjectiveHaskellMini.ObjC

foreign import ccall safe "dynamic" utf8String_dyn
   :: FunPtr (UnsafeId -> Sel -> IO (Ptr CChar))
     -> UnsafeId -> Sel -> IO (Ptr CChar)
utf8String :: Id -> IO (Ptr CChar)
utf8String self = do
    _cmd <- selector "UTF8String";
    withUnsafeId self (\uSelf -> 
        (utf8String_dyn (castFunPtr p_objc_msgSend)) uSelf _cmd)

foreign import ccall safe "dynamic" stringWithUtf8String_dyn
   :: FunPtr (UnsafeId -> Sel -> Ptr CChar -> IO UnsafeId)
     -> UnsafeId -> Sel -> Ptr CChar -> IO UnsafeId
stringWithUtf8String :: Ptr CChar -> Class -> IO Id
stringWithUtf8String charPtr self = do 
    _cmd <- selector "stringWithUTF8String:"
    ((withUnsafeId self (\uSelf -> 
        (stringWithUtf8String_dyn (castFunPtr p_objc_msgSend)) uSelf _cmd charPtr))
          >>= retainedId)

-- | Converts an @NSString@ into a lazy 'String' value.
-- | Note that this /does not/ reuse the internal storage of the @NSString@, and so may not be suitable for large strings.
fromNSString :: Id -> IO String
fromNSString obj = do
    ptr <- obj @. utf8String
    arr <- peekArray0 0 (castPtr ptr) :: IO [Word8]

    return $ toString $ ByteString.pack arr

-- | Converts a 'String' value into an immutable @NSString@.
toNSString :: String -> IO Id
toNSString txt =
    let arr = (ByteString.unpack $ fromString txt) ++ [0]
    in withArray arr $ \ptr ->
        getClass "NSString" >>= stringWithUtf8String (castPtr ptr)

instance Bridged String where
    toObjC = toNSString
    fromObjC = fromNSString

fromNSStringObjC :: Id -> IO (StablePtr String)
fromNSStringObjC obj = fromNSString obj >>= newStablePtr

toNSStringObjC :: StablePtr String -> IO Id
toNSStringObjC ptr = deRefStablePtr ptr >>= toNSString

foreign export ccall "OHHaskellPtrFromNSString" _hs_OHHaskellPtrFromNSString :: UnsafeId -> IO (StablePtr String)
_hs_OHHaskellPtrFromNSString :: UnsafeId -> IO (StablePtr String)
_hs_OHHaskellPtrFromNSString = fromNSStringObjC <=< retainedId

foreign export ccall "OHNSStringFromHaskellPtr" _hs_OHNSStringFromHaskellPtr :: StablePtr String -> IO UnsafeId
_hs_OHNSStringFromHaskellPtr :: StablePtr String -> IO UnsafeId
_hs_OHNSStringFromHaskellPtr = autorelease <=< toNSStringObjC
