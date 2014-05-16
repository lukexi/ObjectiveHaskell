{-# LANGUAGE Trustworthy #-}

-- | Objective-C bridging primitives
module ObjectiveHaskell.ObjC (
        Sel, Class, Id, UnsafeId,
        ObjCBool, NSUInteger,
        Bridged, toObjC, fromObjC,
        selector, getClass,
        retainedId, unretainedId, nil, autorelease, withUnsafeId,
        p_objc_msgSend, (@.)
    ) where

import Control.Applicative
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

-- | An Objective-C @BOOL@.
type ObjCBool = CSChar

-- | Equivalent to Cocoa's @NSUInteger@.
type NSUInteger = CSize

-- | An Objective-C @SEL@.
type Sel = Ptr ()

-- | An object reference that is not being memory-managed by Haskell.
-- | This type should only be used at bridging points -- 'Id' is better for almost every other case.
type UnsafeId = Ptr ()

-- | Represents an Objective-C @IMP@ (although it is not variadic).
type Imp = FunPtr (UnsafeId -> Sel -> IO UnsafeId)

-- | An object reference that is being memory-managed by Haskell.
-- | Objects of type Id will be retained for as long as any Haskell code holds a reference.
newtype Id = Id (ForeignPtr ())
    deriving (Eq, Show, Ord)

-- | As in Objective-C, a Class is just an 'Id', but better documents its purpose in an function signature.
type Class = Id

-- | Represents any value that can be bridged to and from Objective-C.
class Bridged a where
    fromObjC :: Id -> IO a
    toObjC :: a -> IO Id

instance Bridged Id where
    fromObjC = return
    toObjC = return

instance Bridged a => Bridged (Maybe a) where
    toObjC mv = maybe nil toObjC mv
    fromObjC obj =
        withUnsafeId obj $ \ptr ->
            if ptr == nullPtr
                then return Nothing
                else Just <$> fromObjC obj

-- | An 'Id' value representing Objective-C @nil@.
nil :: IO Id
nil = unretainedId nullPtr

-- | Retains an 'UnsafeId' and converts it into an 'Id', which will be released when the last reference to it disappears.
retainedId :: UnsafeId -> IO Id
retainedId obj
    | obj == nullPtr = nil
    | otherwise = Id <$> newForeignPtr p_release obj <* retain obj

-- | Converts an 'UnsafeId' into an 'Id', without any memory management.
-- | This should only be used for objects that do not need to be retained (like 'Class' objects).
unretainedId :: UnsafeId -> IO Id
unretainedId obj = Id <$> newForeignPtr_ obj

-- | Retains then autoreleases an 'Id', returning an 'UnsafeId'.
-- | The resulting 'UnsafeId' can be used safely (by Haskell or Objective-C code) until the autorelease pool is drained.
autorelease :: Id -> IO UnsafeId
autorelease obj =
    withUnsafeId obj $ \obj ->
        if obj == nullPtr
        then return $ obj
        else do
            u <- retain obj
            sel <- selector "autorelease"
            autorelease_dyn (castFunPtr p_objc_msgSend) u sel

-- | Applies a function to the 'UnsafeId' corresponding to an 'Id'.
-- | This can be used to temporarily manipulate an 'UnsafeId' without sacrificing safety.
withUnsafeId
    :: Id                   -- ^ The object to manipulate as an 'UnsafeId'.
    -> (UnsafeId -> IO a)   -- ^ An action to apply to the unwrapped 'UnsafeId'.
    -> IO a                 -- ^ The action returned by the function.

withUnsafeId (Id fptr) = withForeignPtr fptr

-- | Registers a selector with the Objective-C runtime.
selector
    :: String   -- ^ A string representing the selector to register.
    -> IO Sel   -- ^ A new or existing unique selector for the given string.

selector s = withCString s sel_registerName

-- | Returns the Objective-C class by a given name.
getClass :: String -> IO Class
getClass name =
    withCString name $ \name ->
        objc_getClass name >>= unretainedId

{-|

Operator to simplify and clarify messaging syntax. This code:

@
    somethingWithTwoArguments :: Id -> Id -> Id -> IO Id
    somethingWithTwoArguments a b str = …

    f :: Id -> Id -> Id -> IO Id
    f str a b = somethingWithTwoArguments a b str
@

becomes:

@
    f str a b = str @. somethingWithTwoArguments a b
@

-}
(@.) :: Id -> (Id -> b) -> b
(@.) = flip ($)

-- Objective-C runtime functions

-- | A pointer to an unspecialized objc_msgSend.
-- | This function pointer should not be invoked before being cast to the appropriate type.
foreign import ccall safe "objc/runtime.h &objc_msgSend"
    p_objc_msgSend :: Imp

foreign import ccall safe "objc/runtime.h sel_registerName"
    sel_registerName :: CString -> IO Sel

foreign import ccall safe "objc/runtime.h objc_getClass"
    objc_getClass :: CString -> IO UnsafeId

foreign import ccall safe "CoreFoundation/CoreFoundation.h CFRetain"
    retain :: UnsafeId -> IO UnsafeId

foreign import ccall safe "CoreFoundation/CoreFoundation.h &CFRelease"
    p_release :: FunPtr (UnsafeId -> IO ())

-- | Creates a trampoline function from a function pointer that matches the type of @-autorelease@.
foreign import ccall safe "dynamic"
    autorelease_dyn :: FunPtr (UnsafeId -> Sel -> IO UnsafeId) -> (UnsafeId -> Sel -> IO UnsafeId)
