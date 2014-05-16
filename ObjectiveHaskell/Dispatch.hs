{-# LANGUAGE ForeignFunctionInterface #-}
module ObjectiveHaskell.Dispatch where
import Foreign.StablePtr
import ObjectiveHaskell.ObjC

type IOActionPtr = StablePtr (IO ())


foreign export ccall "HSRunAction" runAction :: IOActionPtr -> IO ()
runAction actionPtr = do
    action <- deRefStablePtr actionPtr
    action
    freeStablePtr actionPtr


foreign import ccall safe "OHThreading.h runActionPtrOnDispatchQueue"
    runActionPtrOnDispatchQueue :: UnsafeId -> IOActionPtr -> IO ()

runOnDispatchQueue :: Id -> IO () -> IO ()
runOnDispatchQueue queue action = withUnsafeId queue $ \queue -> 
    runActionPtrOnDispatchQueue queue =<< newStablePtr action


foreign import ccall safe "OHThreading.h runActionPtrOnOperationQueue"
    runActionPtrOnOperationQueue :: UnsafeId -> IOActionPtr -> IO ()

runOnOperationQueue :: Id -> IO () -> IO ()
runOnOperationQueue queue action = withUnsafeId queue $ \queue -> 
    runActionPtrOnOperationQueue queue =<< newStablePtr action
