{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Bridging to and from @NSArray@
module ObjectiveHaskellMini.NSArray (
        fromNSArray, toNSArray
    ) where

import Control.Monad
import Data.Foldable
import Data.Sequence as Seq
import Foreign.C.Types
import Foreign.StablePtr
import Foreign.Ptr
import ObjectiveHaskellMini.ObjC

-- NSArray methods
foreign import ccall safe "dynamic" array_dyn
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
array :: Class -> IO Id
array self = do
    _cmd <- selector "array";
    ((withUnsafeId self (\uSelf -> 
        array_dyn (castFunPtr p_objc_msgSend) uSelf _cmd))
        >>= retainedId)

foreign import ccall safe "dynamic" addObject_dyn
   :: FunPtr (UnsafeId -> Sel -> UnsafeId -> IO ())
     -> UnsafeId -> Sel -> UnsafeId -> IO ()
addObject :: Id -> Id -> IO ()
addObject object self = do 
    _cmd <- selector "addObject:";
    withUnsafeId self (\uSelf -> 
        withUnsafeId object (\uObject -> 
            addObject_dyn (castFunPtr p_objc_msgSend) uSelf _cmd uObject))

foreign import ccall safe "dynamic" copy_dyn
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
copy :: Id -> IO Id
copy self = do
    _cmd <- selector "copy";
    (withUnsafeId self (\uSelf -> 
        copy_dyn (castFunPtr p_objc_msgSend) uSelf _cmd))
        >>= retainedId

foreign import ccall safe "dynamic" count_dyn
   :: FunPtr (UnsafeId -> Sel -> IO NSUInteger)
     -> UnsafeId -> Sel -> IO NSUInteger
count :: Id -> IO NSUInteger
count self = do 
    _cmd <- selector "count";
    withUnsafeId self (\uSelf -> 
        count_dyn (castFunPtr p_objc_msgSend) uSelf _cmd)

foreign import ccall safe "dynamic" objectAtIndex_dyn
   :: FunPtr (UnsafeId -> Sel -> NSUInteger -> IO UnsafeId)
     -> UnsafeId -> Sel -> NSUInteger -> IO UnsafeId
objectAtIndex :: NSUInteger -> Id -> IO Id
objectAtIndex anIndex self = do
    _cmd <- selector "objectAtIndex:";
    (withUnsafeId self (\uSelf -> 
        objectAtIndex_dyn (castFunPtr p_objc_msgSend) uSelf _cmd anIndex))
        >>= retainedId

-- | Converts an @NSArray@ into a 'Seq'.
fromNSArray :: Id -> IO (Seq Id)
fromNSArray arr = do
    c <- toInteger `liftM` count arr
    
    -- TODO: This should use something like fast enumeration instead (blocked on issue #1)
    foldM (\s i -> do
        obj <- arr @. objectAtIndex (fromInteger i)
        return $ s |> obj) empty [0..c-1]

-- | Converts a 'Seq' into an immutable @NSArray@.
toNSArray :: Seq Id -> IO Id
toNSArray s = do
    arr <- getClass "NSMutableArray" >>= array
    mapM (\obj -> arr @. addObject obj) $ toList s

    copy arr

instance Bridged (Seq Id) where
    toObjC = toNSArray
    fromObjC = fromNSArray

fromNSArrayObjC :: Id -> IO (StablePtr (Seq Id))
fromNSArrayObjC obj = fromNSArray obj >>= newStablePtr

toNSArrayObjC :: StablePtr (Seq Id) -> IO Id
toNSArrayObjC ptr = deRefStablePtr ptr >>= toNSArray

foreign export ccall "OHHaskellPtrFromNSArray" _hs_OHHaskellPtrFromNSArray :: UnsafeId -> IO (StablePtr (Seq Id))
_hs_OHHaskellPtrFromNSArray :: UnsafeId -> IO (StablePtr (Seq Id))
_hs_OHHaskellPtrFromNSArray = fromNSArrayObjC <=< retainedId

foreign export ccall "OHNSArrayFromHaskellPtr" _hs_OHNSArrayFromHaskellPtr :: StablePtr (Seq Id) -> IO UnsafeId
_hs_OHNSArrayFromHaskellPtr :: StablePtr (Seq Id) -> IO UnsafeId
_hs_OHNSArrayFromHaskellPtr = autorelease <=< toNSArrayObjC
