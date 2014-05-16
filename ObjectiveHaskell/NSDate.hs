{-# LANGUAGE Trustworthy #-}

-- | Bridging from UTCTime to @NSDate@
module ObjectiveHaskell.NSDate (
        toNSDate
    ) where

import Control.Applicative
import Data.ByteString.Lazy as ByteString
import Data.Word
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import ObjectiveHaskell.ObjC

-- NSDate methods
foreign import ccall safe "dynamic" dateWithTimeIntervalSince1970_dyn
  :: FunPtr (UnsafeId -> Sel -> CDouble -> IO UnsafeId)
  -> UnsafeId
  -> Sel
  -> CDouble
  -> IO UnsafeId
dateWithTimeIntervalSince1970 :: CDouble -> Class -> IO Id
dateWithTimeIntervalSince1970 interval self = do
    { _cmd <- selector "dateWithTimeIntervalSince1970:";
         ((withUnsafeId
             self
             (\ self
                -> (dateWithTimeIntervalSince1970_dyn (castFunPtr p_objc_msgSend))
                     self _cmd interval))
          >>= retainedId) }

-- | Converts a UTCTime into an immutable @NSDate@ object.
toNSDate :: UTCTime -> IO Id
toNSDate time = getClass "NSDate" >>= dateWithTimeIntervalSince1970 (realToFrac $ utcTimeToPOSIXSeconds time)
