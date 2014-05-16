{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}

-- | Bridging to and from @NSNumber@
module ObjectiveHaskell.NSNumber (
        fromNSNumber, toNSNumber
    ) where

import Control.Applicative
import Data.Char
import Data.Ratio
import Foreign.C.String
import Foreign.C.Types
import Foreign.StablePtr
import Foreign.Ptr
import ObjectiveHaskell.ObjC
import ObjectiveHaskell.NSString

-- NSNumber methods
foreign import ccall safe "dynamic" numberWithLongLong_dyn
   :: FunPtr (UnsafeId -> Sel -> CLLong -> IO UnsafeId)
     -> UnsafeId -> Sel -> CLLong -> IO UnsafeId
numberWithLongLong :: CLLong -> Class -> IO Id
numberWithLongLong longNum self
  = do { _cmd <- selector "numberWithLongLong:";
         ((withUnsafeId
             self
             (\ self
                -> (numberWithLongLong_dyn (castFunPtr p_objc_msgSend))
                     self _cmd longNum))
          >>= retainedId) }
foreign import ccall safe "dynamic" numberWithUnsignedLongLong_dyn
   :: FunPtr (UnsafeId -> Sel -> CULLong -> IO UnsafeId)
     -> UnsafeId -> Sel -> CULLong -> IO UnsafeId
numberWithUnsignedLongLong :: CULLong -> Class -> IO Id
numberWithUnsignedLongLong longNum self
  = do { _cmd <- selector "numberWithUnsignedLongLong:";
         ((withUnsafeId
             self
             (\ self
                -> (numberWithUnsignedLongLong_dyn
                      (castFunPtr p_objc_msgSend))
                     self _cmd longNum))
          >>= retainedId) }
foreign import ccall safe "dynamic" numberWithDouble_dyn_aURo
   :: FunPtr (UnsafeId -> Sel -> CDouble -> IO UnsafeId)
     -> UnsafeId -> Sel -> CDouble -> IO UnsafeId
numberWithDouble :: CDouble -> Class -> IO Id
numberWithDouble double self
  = do { _cmd <- selector "numberWithDouble:";
         ((withUnsafeId
             self
             (\ self
                -> (numberWithDouble_dyn_aURo (castFunPtr p_objc_msgSend))
                     self _cmd double))
          >>= retainedId) }

foreign import ccall safe "dynamic" doubleValue_dyn_aUSh
   :: FunPtr (UnsafeId -> Sel -> IO CDouble)
     -> UnsafeId -> Sel -> IO CDouble
doubleValue :: Id -> IO CDouble
doubleValue self
  = do { _cmd <- selector "doubleValue";
         withUnsafeId
           self
           (\ self
              -> (doubleValue_dyn_aUSh (castFunPtr p_objc_msgSend)) self _cmd) }
foreign import ccall safe "dynamic" longLongValue_dyn_aUT7
   :: FunPtr (UnsafeId -> Sel -> IO CLLong)
     -> UnsafeId -> Sel -> IO CLLong
longLongValue :: Id -> IO CLLong
longLongValue self
  = do { _cmd <- selector "longLongValue";
         withUnsafeId
           self
           (\ self
              -> (longLongValue_dyn_aUT7 (castFunPtr p_objc_msgSend))
                   self _cmd) }
foreign import ccall safe "dynamic" unsignedLongLongValue_dyn_aUTX
   :: FunPtr (UnsafeId -> Sel -> IO CULLong)
     -> UnsafeId -> Sel -> IO CULLong
unsignedLongLongValue :: Id -> IO CULLong
unsignedLongLongValue self
  = do { _cmd <- selector "unsignedLongLongValue";
         withUnsafeId
           self
           (\ self
              -> (unsignedLongLongValue_dyn_aUTX (castFunPtr p_objc_msgSend))
                   self _cmd) }
foreign import ccall safe "dynamic" objCType_dyn_aUUN
   :: FunPtr (UnsafeId -> Sel -> IO CString)
     -> UnsafeId -> Sel -> IO CString
objCType :: Id -> IO CString
objCType self
  = do { _cmd <- selector "objCType";
         withUnsafeId
           self
           (\ self
              -> (objCType_dyn_aUUN (castFunPtr p_objc_msgSend)) self _cmd) }

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

foreign export ccall "OHHaskellPtrFromNSNumber" _hs_OHHaskellPtrFromNSNumber
  :: UnsafeId -> IO (StablePtr Rational)
_hs_OHHaskellPtrFromNSNumber number
  = do { number <- retainedId number;
         fromNSNumberObjC number }
foreign export ccall "OHNSNumberFromHaskellPtr" _hs_OHNSNumberFromHaskellPtr
  :: StablePtr Rational -> IO UnsafeId
_hs_OHNSNumberFromHaskellPtr ratPtr
  = do { result_aUY4 <- toNSNumberObjC ratPtr;
         autorelease result_aUY4 }
