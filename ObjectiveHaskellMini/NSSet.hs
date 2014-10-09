{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Bridging to and from @NSSet@
module ObjectiveHaskellMini.NSSet (
        fromNSSet, toNSSet
    ) where

import Control.Monad
import qualified Data.Set as Set
import Data.Set (Set)
import Foreign.C.Types
import Foreign.StablePtr
import Foreign.Ptr
import ObjectiveHaskellMini.ObjC

-- NSSet methods
foreign import ccall safe "dynamic" set_dyn
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
set :: Class -> IO Id
set self = do
    _cmd <- selector "set"
    (withUnsafeId self (\uSelf ->
        set_dyn (castFunPtr p_objc_msgSend) uSelf _cmd))
        >>= retainedId

foreign import ccall safe "dynamic" addObject_dyn
   :: FunPtr (UnsafeId -> Sel -> UnsafeId -> IO ())
     -> UnsafeId -> Sel -> UnsafeId -> IO ()
addObject :: Id -> Id -> IO ()
addObject obj self = do
    _cmd <- selector "addObject:"
    withUnsafeId self $ \uSelf ->
        withUnsafeId obj $ \uObj ->
            addObject_dyn (castFunPtr p_objc_msgSend) uSelf _cmd uObj

foreign import ccall safe "dynamic" copy_dyn
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
copy :: Id -> IO Id
copy self = do
    _cmd <- selector "copy"
    (withUnsafeId self (\uSelf ->
        copy_dyn (castFunPtr p_objc_msgSend) uSelf _cmd))
        >>= retainedId

foreign import ccall safe "dynamic" allObjects_dyn
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
allObjects :: Id -> IO Id
allObjects self = do
    _cmd <- selector "allObjects"
    (withUnsafeId self (\uSelf ->
        allObjects_dyn (castFunPtr p_objc_msgSend) uSelf _cmd))
        >>= retainedId

foreign import ccall safe "dynamic" count_dyn
   :: FunPtr (UnsafeId -> Sel -> IO NSUInteger)
     -> UnsafeId -> Sel -> IO NSUInteger
count :: Id -> IO NSUInteger
count self = do
    _cmd <- selector "count";
    withUnsafeId self $ \uSelf ->
        count_dyn (castFunPtr p_objc_msgSend) uSelf _cmd

-- NSArray methods
foreign import ccall safe "dynamic" objectAtIndex_dyn
   :: FunPtr (UnsafeId -> Sel -> NSUInteger -> IO UnsafeId)
     -> UnsafeId -> Sel -> NSUInteger -> IO UnsafeId
objectAtIndex :: NSUInteger -> Id -> IO Id
objectAtIndex i self = do
    _cmd <- selector "objectAtIndex:";
    (withUnsafeId self (\uSelf ->
        objectAtIndex_dyn (castFunPtr p_objc_msgSend) uSelf _cmd i))
        >>= retainedId

-- | Converts an @NSSet@ into a 'Set'.
fromNSSet :: (Ord e, Bridged e) => Id -> IO (Set e)
fromNSSet aSet = do
    c <- toInteger `liftM` count aSet
    setAsArray <- allObjects aSet

    -- TODO: This should use something like fast enumeration instead (blocked on issue #1)
    foldM (\setSoFar i -> do
        obj <- fromObjC =<< setAsArray @. objectAtIndex (fromInteger i)
        return $ Set.insert obj setSoFar) Set.empty [0..c-1]

-- | Converts a 'Set' into an immutable @NSSet@.
toNSSet :: (Ord e, Bridged e) => Set e -> IO Id
toNSSet s = do
    aSet <- getClass "NSMutableSet" >>= set
    mapM_ (\obj -> do {objId <- toObjC obj; aSet @. addObject objId}) $ Set.toList s
    copy aSet

instance (Ord e, Bridged e) => Bridged (Set e) where
    toObjC = toNSSet
    fromObjC = fromNSSet

fromNSSetObjC :: Id -> IO (StablePtr (Set Id))
fromNSSetObjC obj = fromNSSet obj >>= newStablePtr

toNSSetObjC :: StablePtr (Set Id) -> IO Id
toNSSetObjC ptr = deRefStablePtr ptr >>= toNSSet

foreign export ccall "OHHaskellPtrFromNSSet" _hs_OHHaskellPtrFromNSSet :: UnsafeId -> IO (StablePtr (Set Id))
_hs_OHHaskellPtrFromNSSet :: UnsafeId -> IO (StablePtr (Set Id))
_hs_OHHaskellPtrFromNSSet = fromNSSetObjC <=< retainedId

foreign export ccall "OHNSSetFromHaskellPtr" _hs_OHNSSetFromHaskellPtr :: StablePtr (Set Id) -> IO UnsafeId
_hs_OHNSSetFromHaskellPtr :: StablePtr (Set Id) -> IO UnsafeId
_hs_OHNSSetFromHaskellPtr = autorelease <=< toNSSetObjC
