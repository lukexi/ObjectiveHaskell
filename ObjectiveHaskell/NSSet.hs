{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}

-- | Bridging to and from @NSSet@
module ObjectiveHaskell.NSSet (
        fromNSSet, toNSSet
    ) where

import Control.Monad
import Data.Foldable
import qualified Data.Set as Set
import Data.Set (Set)
import Foreign.C.Types
import Foreign.StablePtr
import Foreign.Ptr
import ObjectiveHaskell.ObjC

-- NSSet methods
foreign import ccall safe "dynamic" set_dyn_aVJ5
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
set :: Class -> IO Id
set self
  = do { _cmd <- selector "set";
         ((withUnsafeId
             self
             (\ self -> (set_dyn_aVJ5 (castFunPtr p_objc_msgSend)) self _cmd))
          >>= retainedId) }
foreign import ccall safe "dynamic" addObject_dyn_aVK6
   :: FunPtr (UnsafeId -> Sel -> UnsafeId -> IO ())
     -> UnsafeId -> Sel -> UnsafeId -> IO ()
addObject :: Id -> Id -> IO ()
addObject a_aVK5 self
  = do { _cmd <- selector "addObject:";
         withUnsafeId
           self
           (\ self
              -> withUnsafeId
                   a_aVK5
                   (\ a_aVK5
                      -> (addObject_dyn_aVK6 (castFunPtr p_objc_msgSend))
                           self _cmd a_aVK5)) }
foreign import ccall safe "dynamic" copy_dyn_aVKW
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
copy :: Id -> IO Id
copy self
  = do { _cmd <- selector "copy";
         ((withUnsafeId
             self
             (\ self -> (copy_dyn_aVKW (castFunPtr p_objc_msgSend)) self _cmd))
          >>= retainedId) }
foreign import ccall safe "dynamic" allObjects_dyn_aVLO
   :: FunPtr (UnsafeId -> Sel -> IO UnsafeId)
     -> UnsafeId -> Sel -> IO UnsafeId
allObjects :: Id -> IO Id
allObjects self
  = do { _cmd <- selector "allObjects";
         ((withUnsafeId
             self
             (\ self
                -> (allObjects_dyn_aVLO (castFunPtr p_objc_msgSend)) self _cmd))
          >>= retainedId) }
foreign import ccall safe "dynamic" count_dyn_aVMG
   :: FunPtr (UnsafeId -> Sel -> IO NSUInteger)
     -> UnsafeId -> Sel -> IO NSUInteger
count :: Id -> IO NSUInteger
count self
  = do { _cmd <- selector "count";
         withUnsafeId
           self
           (\ self
              -> (count_dyn_aVMG (castFunPtr p_objc_msgSend)) self _cmd) }

-- NSArray methods
foreign import ccall safe "dynamic" objectAtIndex_dyn_aVNL
   :: FunPtr (UnsafeId -> Sel -> NSUInteger -> IO UnsafeId)
     -> UnsafeId -> Sel -> NSUInteger -> IO UnsafeId
objectAtIndex :: NSUInteger -> Id -> IO Id
objectAtIndex a_aVNK self
  = do { _cmd <- selector "objectAtIndex:";
         ((withUnsafeId
             self
             (\ self
                -> (objectAtIndex_dyn_aVNL (castFunPtr p_objc_msgSend))
                     self _cmd a_aVNK))
          >>= retainedId) }

-- | Converts an @NSSet@ into a 'Set'.
fromNSSet :: Id -> IO (Set Id)
fromNSSet aSet = do
    c <- toInteger `liftM` count aSet
    setAsArray <- allObjects aSet
    
    -- TODO: This should use something like fast enumeration instead (blocked on issue #1)
    foldM (\set i -> do
        obj <- setAsArray @. objectAtIndex (fromInteger i)
        return $ Set.insert obj set) Set.empty [0..c-1]

-- | Converts a 'Set' into an immutable @NSSet@.
toNSSet :: Set Id -> IO Id
toNSSet s = do
    aSet <- getClass "NSMutableSet" >>= set
    mapM (\obj -> aSet @. addObject obj) $ Set.toList s

    copy aSet

instance Bridged (Set Id) where
    toObjC = toNSSet
    fromObjC = fromNSSet

fromNSSetObjC :: Id -> IO (StablePtr (Set Id))
fromNSSetObjC obj = fromNSSet obj >>= newStablePtr

toNSSetObjC :: StablePtr (Set Id) -> IO Id
toNSSetObjC ptr = deRefStablePtr ptr >>= toNSSet

foreign export ccall "OHHaskellPtrFromNSSet" _hs_OHHaskellPtrFromNSSet_aVQ1
  :: UnsafeId -> IO (StablePtr (Set Id))
_hs_OHHaskellPtrFromNSSet_aVQ1 a_aVPZ
  = do { a_aVPZ <- retainedId a_aVPZ;
         fromNSSetObjC a_aVPZ }
foreign export ccall "OHNSSetFromHaskellPtr" _hs_OHNSSetFromHaskellPtr_aVRq
  :: StablePtr (Set Id) -> IO UnsafeId
_hs_OHNSSetFromHaskellPtr_aVRq a_aVRo
  = do { result_aVRp <- toNSSetObjC a_aVRo;
         autorelease result_aVRp }
