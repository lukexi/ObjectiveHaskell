{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Bridging to and from @NSNumber@
module ObjectiveHaskellMini.NSNumber (
        fromNSNumber, toNSNumber
    ) where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.Ratio
import Foreign.C.String
import Foreign.C.Types
import Foreign.StablePtr
import Foreign.Ptr
import ObjectiveHaskellMini.ObjC

-- NSNumber methods
foreign import ccall safe "dynamic" numberWithLongLong_dyn
   :: FunPtr (UnsafeId -> Sel -> CLLong -> IO UnsafeId)
     -> UnsafeId -> Sel -> CLLong -> IO UnsafeId
numberWithLongLong :: CLLong -> Class -> IO Id
numberWithLongLong longNum self = do 
    _cmd <- selector "numberWithLongLong:"
    (withUnsafeId self (\uSelf -> 
        numberWithLongLong_dyn (castFunPtr p_objc_msgSend) uSelf _cmd longNum))
        >>= retainedId

foreign import ccall safe "dynamic" numberWithUnsignedLongLong_dyn
   :: FunPtr (UnsafeId -> Sel -> CULLong -> IO UnsafeId)
     -> UnsafeId -> Sel -> CULLong -> IO UnsafeId
numberWithUnsignedLongLong :: CULLong -> Class -> IO Id
numberWithUnsignedLongLong longNum self = do
    _cmd <- selector "numberWithUnsignedLongLong:";
    (withUnsafeId self (\uSelf -> 
        numberWithUnsignedLongLong_dyn (castFunPtr p_objc_msgSend) uSelf _cmd longNum))
        >>= retainedId

foreign import ccall safe "dynamic" numberWithDouble_dyn
   :: FunPtr (UnsafeId -> Sel -> CDouble -> IO UnsafeId)
     -> UnsafeId -> Sel -> CDouble -> IO UnsafeId
numberWithDouble :: CDouble -> Class -> IO Id
numberWithDouble double self = do
    _cmd <- selector "numberWithDouble:"
    (withUnsafeId self (\uSelf -> 
        numberWithDouble_dyn (castFunPtr p_objc_msgSend) uSelf _cmd double))
        >>= retainedId

foreign import ccall safe "dynamic" doubleValue_dyn
   :: FunPtr (UnsafeId -> Sel -> IO CDouble)
     -> UnsafeId -> Sel -> IO CDouble
doubleValue :: Id -> IO CDouble
doubleValue self = do
    _cmd <- selector "doubleValue"
    withUnsafeId self $ \uSelf -> 
        doubleValue_dyn (castFunPtr p_objc_msgSend) uSelf _cmd

foreign import ccall safe "dynamic" longLongValue_dyn
   :: FunPtr (UnsafeId -> Sel -> IO CLLong)
     -> UnsafeId -> Sel -> IO CLLong
longLongValue :: Id -> IO CLLong
longLongValue self = do
    _cmd <- selector "longLongValue"
    withUnsafeId self $ \uSelf ->
        longLongValue_dyn (castFunPtr p_objc_msgSend) uSelf _cmd

foreign import ccall safe "dynamic" unsignedLongLongValue_dyn
   :: FunPtr (UnsafeId -> Sel -> IO CULLong)
     -> UnsafeId -> Sel -> IO CULLong
unsignedLongLongValue :: Id -> IO CULLong
unsignedLongLongValue self = do 
    _cmd <- selector "unsignedLongLongValue"
    withUnsafeId self $ \uSelf -> 
        unsignedLongLongValue_dyn (castFunPtr p_objc_msgSend) uSelf _cmd

foreign import ccall safe "dynamic" objCType_dyn
   :: FunPtr (UnsafeId -> Sel -> IO CString)
     -> UnsafeId -> Sel -> IO CString
objCType :: Id -> IO CString
objCType self = do 
    _cmd <- selector "objCType";
    withUnsafeId self $ \uSelf -> 
        (objCType_dyn (castFunPtr p_objc_msgSend)) uSelf _cmd

-- | Converts an @NSNumber@ into a 'Rational'.
fromNSNumber :: Id -> IO Rational
fromNSNumber obj = do
    t <- obj @. objCType >>= peekCString
    
    if (t == "f") || (t == "d")
    then toRational <$> obj @. doubleValue

    -- Signed type encodings are lowercase.
    else if isLower $ Prelude.head t
            then toRational <$> obj @. longLongValue
            else toRational <$> obj @. unsignedLongLongValue

-- | Converts a 'Rational' into an @NSNumber@.
toNSNumber :: Rational -> IO Id
toNSNumber rat
    | denominator rat /= 1 = getClass "NSNumber" >>= numberWithDouble (fromRational rat)
    | numerator rat < 0 = getClass "NSNumber" >>= numberWithLongLong (fromIntegral $ numerator rat)
    | otherwise = getClass "NSNumber" >>= numberWithUnsignedLongLong (fromIntegral $ numerator rat)

instance Bridged Rational where
    toObjC = toNSNumber
    fromObjC = fromNSNumber

fromNSNumberObjC :: Id -> IO (StablePtr Rational)
fromNSNumberObjC obj = fromNSNumber obj >>= newStablePtr

toNSNumberObjC :: StablePtr Rational -> IO Id
toNSNumberObjC ptr = deRefStablePtr ptr >>= toNSNumber

foreign export ccall "OHHaskellPtrFromNSNumber" _hs_OHHaskellPtrFromNSNumber :: UnsafeId -> IO (StablePtr Rational)
_hs_OHHaskellPtrFromNSNumber :: UnsafeId -> IO (StablePtr Rational)
_hs_OHHaskellPtrFromNSNumber = fromNSNumberObjC <=< retainedId

foreign export ccall "OHNSNumberFromHaskellPtr" _hs_OHNSNumberFromHaskellPtr :: StablePtr Rational -> IO UnsafeId
_hs_OHNSNumberFromHaskellPtr :: StablePtr Rational -> IO UnsafeId
_hs_OHNSNumberFromHaskellPtr = autorelease <=< toNSNumberObjC 
