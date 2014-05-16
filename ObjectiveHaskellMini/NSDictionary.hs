{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Bridging to and from @NSDictionary@
module ObjectiveHaskellMini.NSDictionary (
        fromNSDictionary, toNSDictionary
    ) where

import Control.Applicative
import Control.Monad
import Data.Foldable as Foldable
import Data.Map as Map hiding (keys)
import Foreign.StablePtr
import Foreign.Ptr
import ObjectiveHaskellMini.ObjC
import ObjectiveHaskellMini.NSArray

-- NSDictionary methods
foreign import ccall safe "dynamic" allKeys_dyn
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
allKeys :: Id -> IO Id
allKeys self = do
    _cmd <- selector "allKeys";
    (withUnsafeId self (\uSelf -> allKeys_dyn (castFunPtr p_objc_msgSend) uSelf _cmd))
        >>= retainedId

foreign import ccall safe "dynamic" copy_dyn
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
copy :: Id -> IO Id
copy self = do
    _cmd <- selector "copy";
    (withUnsafeId self (\uSelf -> copy_dyn (castFunPtr p_objc_msgSend) uSelf _cmd))
        >>= retainedId

foreign import ccall safe "dynamic" dictionary_dyn
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
dictionary :: Class -> IO Id
dictionary self = do
    _cmd <- selector "dictionary";
    (withUnsafeId self (\uSelf -> dictionary_dyn (castFunPtr p_objc_msgSend) uSelf _cmd))
        >>= retainedId

foreign import ccall safe "dynamic" objectForKey_dyn
   :: FunPtr (UnsafeId -> Sel -> UnsafeId -> IO UnsafeId)
     -> UnsafeId -> Sel -> UnsafeId -> IO UnsafeId
objectForKey :: Id -> Id -> IO Id
objectForKey key self = do
    _cmd <- selector "objectForKey:";
    (withUnsafeId self (\uSelf -> 
        withUnsafeId key (\uKey -> 
            objectForKey_dyn (castFunPtr p_objc_msgSend) uSelf _cmd uKey)))
        >>= retainedId

foreign import ccall safe "dynamic" setObjectForKey_dyn
   :: FunPtr (UnsafeId -> Sel -> UnsafeId -> UnsafeId -> IO ())
     -> UnsafeId -> Sel -> UnsafeId -> UnsafeId -> IO ()
setObjectForKey :: Id -> Id -> Id -> IO ()
setObjectForKey object key self = do
    _cmd <- selector "setObject:forKey:";
    withUnsafeId self (\uSelf -> 
        withUnsafeId object (\uObject -> 
            withUnsafeId key (\uKey -> 
                setObjectForKey_dyn (castFunPtr p_objc_msgSend) uSelf _cmd uObject uKey)))

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

-- Allows direct conversion between maps of bridgeable keys and values to NSDictionaries
instance (Ord a, Bridged a, Bridged b) => Bridged (Map a b) where
    toObjC aMap = toObjC =<< toIdMap aMap
    fromObjC mapId = fromIdMap =<< fromObjC mapId

fromIdMap :: (Bridged a, Bridged b, Ord a) => Map Id Id -> IO (Map a b)
fromIdMap aMap = Map.fromList <$> forM (Map.toList aMap) (\(keyId, valueId) -> do
    key   <- fromObjC keyId
    value <- fromObjC valueId
    return (key, value))

toIdMap :: (Bridged a, Bridged b) => Map a b -> IO (Map Id Id)
toIdMap aMap = Map.fromList <$> forM (Map.toList aMap) (\(key, value) -> do
    keyId   <- toObjC key
    valueId <- toObjC value
    return (keyId, valueId))


foreign export ccall "OHHaskellPtrFromNSDictionary" _hs_OHHaskellPtrFromNSDictionary :: UnsafeId -> IO (StablePtr (Map Id Id))
_hs_OHHaskellPtrFromNSDictionary :: UnsafeId -> IO (StablePtr (Map Id Id))
_hs_OHHaskellPtrFromNSDictionary = fromNSDictionaryObjC <=< retainedId

foreign export ccall "OHNSDictionaryFromHaskellPtr" _hs_OHNSDictionaryFromHaskellPtr :: StablePtr (Map Id Id) -> IO UnsafeId
_hs_OHNSDictionaryFromHaskellPtr :: StablePtr (Map Id Id) -> IO UnsafeId
_hs_OHNSDictionaryFromHaskellPtr = autorelease <=< toNSDictionaryObjC

