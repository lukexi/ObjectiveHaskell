{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
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
import ObjectiveHaskell.TH.MsgSend
import ObjectiveHaskell.TH.ObjC

-- NSSet methods
declMessage "set" [t| Class -> IO Id |] "set"
declMessage "addObject" [t| Id -> Id -> IO () |] "addObject:"
declMessage "copy" [t| Id -> IO Id |] "copy"
declMessage "allObjects" [t| Id -> IO Id |] "allObjects"
declMessage "count" [t| Id -> IO NSUInteger |] "count"

-- NSArray methods
declMessage "objectAtIndex" [t| NSUInteger -> Id -> IO Id |] "objectAtIndex:"

-- | Converts an @NSSet@ into a 'Set'.
fromNSSet :: Id -> IO (Set Id)
fromNSSet aSet = do
    c <- count aSet
    setAsArray <- allObjects aSet
    
    -- TODO: This should use something like fast enumeration instead (blocked on issue #1)
    foldM (\set i -> do
        obj <- setAsArray @. objectAtIndex i
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

exportFunc "OHHaskellPtrFromNSSet" [t| UnsafeId -> IO (StablePtr (Set Id)) |] 'fromNSSetObjC
exportFunc "OHNSSetFromHaskellPtr" [t| StablePtr (Set Id) -> IO UnsafeId |] 'toNSSetObjC
