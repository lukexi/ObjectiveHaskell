{-# LANGUAGE ForeignFunctionInterface #-}

module ObjectiveHaskell.ObjC (
        Sel, Class, Id, Imp, nil,
        p_objc_msgSend,
        retain, release,
        selector, getClass
    ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Unsafe
import Foreign.Ptr

nil :: Id
nil = nullPtr

type Sel = Ptr ()
type Class = Ptr ()
type Imp = FunPtr (Id -> Sel -> IO Id)

-- TODO: this should really be a ForeignPtr with retain/release
type Id = Ptr ()

foreign import ccall safe "objc/runtime.h &objc_msgSend"
    p_objc_msgSend :: Imp

foreign import ccall unsafe "objc/runtime.h sel_registerName"
    sel_registerName :: CString -> IO Sel

foreign import ccall unsafe "objc/runtime.h objc_getClass"
    objc_getClass :: CString -> Class

foreign import ccall unsafe "OHMemoryManagement.h OHRetain"
    retain :: Id -> IO Id

foreign import ccall safe "OHMemoryManagement.h OHRelease"
    release :: Id -> IO ()

-- Maps a string to a selector
selector :: String -> IO Sel
selector s = withCString s sel_registerName

-- Returns the class by the given name
getClass :: String -> Class
getClass name =
    -- objc_getClass() is actually pure, but having to do memory allocation puts us in the IO monad
    -- Since the stack allocation has no visible effects outside of this function, we unwrap IO
    unsafeLocalState $ withCString name (return . objc_getClass)
