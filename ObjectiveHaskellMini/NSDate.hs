{-# LANGUAGE Trustworthy #-}

-- | Bridging from UTCTime to @NSDate@
module ObjectiveHaskellMini.NSDate (
        toNSDate
    ) where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Foreign.C.Types
import Foreign.Ptr
import ObjectiveHaskellMini.ObjC

-- NSDate methods
foreign import ccall safe "dynamic" dateWithTimeIntervalSince1970_dyn
  :: FunPtr (UnsafeId -> Sel -> CDouble -> IO UnsafeId)
  -> UnsafeId
  -> Sel
  -> CDouble
  -> IO UnsafeId
dateWithTimeIntervalSince1970 :: CDouble -> Class -> IO Id
dateWithTimeIntervalSince1970 interval self = do
    _cmd <- selector "dateWithTimeIntervalSince1970:"
    (withUnsafeId self (\uSelf -> 
        dateWithTimeIntervalSince1970_dyn (castFunPtr p_objc_msgSend) uSelf _cmd interval))
        >>= retainedId

-- | Converts a UTCTime into an immutable @NSDate@ object.
toNSDate :: UTCTime -> IO Id
toNSDate time = getClass "NSDate" >>= dateWithTimeIntervalSince1970 (realToFrac $ utcTimeToPOSIXSeconds time)
