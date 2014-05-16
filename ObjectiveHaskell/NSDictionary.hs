{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}

-- | Bridging to and from @NSDictionary@
module ObjectiveHaskell.NSDictionary (
        fromNSDictionary, toNSDictionary
    ) where

import Control.Applicative
import Control.Monad
import Data.Foldable as Foldable
import Data.List
import Data.Map as Map
import Foreign.C.Types
import Foreign.StablePtr
import Foreign.Ptr
import ObjectiveHaskell.ObjC
import ObjectiveHaskell.NSArray

-- NSDictionary methods
foreign import ccall safe "dynamic" allKeys_dyn_aTFt
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
allKeys :: Id -> IO Id
allKeys self
  = do { _cmd <- selector "allKeys";
         ((withUnsafeId
             self
             (\ self
                -> (allKeys_dyn_aTFt (castFunPtr p_objc_msgSend)) self _cmd))
          >>= retainedId) }
foreign import ccall safe "dynamic" copy_dyn_aTGl
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
copy :: Id -> IO Id
copy self
  = do { _cmd <- selector "copy";
         ((withUnsafeId
             self
             (\ self -> (copy_dyn_aTGl (castFunPtr p_objc_msgSend)) self _cmd))
          >>= retainedId) }
foreign import ccall safe "dynamic" dictionary_dyn_aTHd
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
dictionary :: Class -> IO Id
dictionary self
  = do { _cmd <- selector "dictionary";
         ((withUnsafeId
             self
             (\ self
                -> (dictionary_dyn_aTHd (castFunPtr p_objc_msgSend)) self _cmd))
          >>= retainedId) }
foreign import ccall safe "dynamic" objectForKey_dyn_aTIk
   :: FunPtr (UnsafeId -> Sel -> UnsafeId -> IO UnsafeId)
     -> UnsafeId -> Sel -> UnsafeId -> IO UnsafeId
objectForKey :: Id -> Id -> IO Id
objectForKey a_aTIj self
  = do { _cmd <- selector "objectForKey:";
         ((withUnsafeId
             self
             (\ self
                -> withUnsafeId
                     a_aTIj
                     (\ a_aTIj
                        -> (objectForKey_dyn_aTIk (castFunPtr p_objc_msgSend))
                             self _cmd a_aTIj)))
          >>= retainedId) }
foreign import ccall safe "dynamic" setObjectForKey_dyn_aTJA
   :: FunPtr (UnsafeId -> Sel -> UnsafeId -> UnsafeId -> IO ())
     -> UnsafeId -> Sel -> UnsafeId -> UnsafeId -> IO ()
setObjectForKey :: Id -> Id -> Id -> IO ()
setObjectForKey a_aTJy a_aTJz self
  = do { _cmd <- selector "setObject:forKey:";
         withUnsafeId
           self
           (\ self
              -> withUnsafeId
                   a_aTJy
                   (\ a_aTJy
                      -> withUnsafeId
                           a_aTJz
                           (\ a_aTJz
                              -> (setObjectForKey_dyn_aTJA (castFunPtr p_objc_msgSend))
                                   self _cmd a_aTJy a_aTJz))) }

-- | Converts an @NSDictionary@ into a 'Map'.
fromNSDictionary :: Id -> IO (Map Id Id)
fromNSDictionary dict = do
    keys <- Foldable.toList <$> (dict @. allKeys >>= fromNSArray)
    vals <- mapM (\k -> dict @. objectForKey k) keys
    
    return $ Map.fromList $ zip keys vals

-- | Converts a 'Map' into an immutable @NSDictionary@.
toNSDictionary :: Map Id Id -> IO Id
toNSDictionary tbl = do
    dict <- getClass "NSMutableDictionary" >>= dictionary
    mapM (\(k, v) -> dict @. setObjectForKey v k) $ Map.toList tbl

    copy dict

instance Bridged (Map Id Id) where
    toObjC = toNSDictionary
    fromObjC = fromNSDictionary

fromNSDictionaryObjC :: Id -> IO (StablePtr (Map Id Id))
fromNSDictionaryObjC obj = fromNSDictionary obj >>= newStablePtr

toNSDictionaryObjC :: StablePtr (Map Id Id) -> IO Id
toNSDictionaryObjC ptr = deRefStablePtr ptr >>= toNSDictionary

foreign export ccall "OHHaskellPtrFromNSDictionary" _hs_OHHaskellPtrFromNSDictionary_aTLU
  :: UnsafeId -> IO (StablePtr (Map Id Id))
_hs_OHHaskellPtrFromNSDictionary_aTLU a_aTLS
  = do { a_aTLS <- retainedId a_aTLS;
         fromNSDictionaryObjC a_aTLS }
foreign export ccall "OHNSDictionaryFromHaskellPtr" _hs_OHNSDictionaryFromHaskellPtr_aTNv
  :: StablePtr (Map Id Id) -> IO UnsafeId
_hs_OHNSDictionaryFromHaskellPtr_aTNv a_aTNt
  = do { result_aTNu <- toNSDictionaryObjC a_aTNt;
         autorelease result_aTNu }
