//
//  NSObject+OHExtensions.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-07-13.
//  Copyright (C) 2013 Justin Spahr-Summers.
//

#import <Foundation/Foundation.h>
#import "HsFFI.h"
#import "Rts.h"

// LXI 2-25-2014: I've disabled this as it causes trouble to have this called at +load time
// when using traceStack, manifesting as a EXC_BAD_ACCESS in enterFunEqualStacks. I'm just calling hs_init at the beginning
// of SPCBridge.m, which works perfectly.
// (calling it in +load of SPCBridge exhibits the same issue, so it must have something to do with when the function is called.)
// UPDATE: calling it in +initialize works perfectly! But that can't be called in a category...

// This doesn't work either, FYI.
// __attribute__((constructor))
// static void startHaskellRuntime() {
  
// }

@interface NSObject (OHExtensions)
@end

/**
 * This private category ensures that the Haskell runtime is loaded at about the
 * same time as the Objective-C runtime.
 */
@implementation NSObject (OHExtensions)

// + (void)load {

    // int argc = 2;
    // char *argv[] = { "+RTS", "-xc", NULL };
    // char **pargv = argv;

    // // Initialize Haskell runtime
    // hs_init(&argc, &pargv);

    // http://www.haskell.org/ghc/docs/latest/html/users_guide/ffi-ghc.html claims this is needed for RTS opts, though it's not clear.
    // RtsConfig conf = defaultRtsConfig;
    // conf.rts_opts_enabled = RtsOptsAll;
    // hs_init_ghc(&argc, &pargv, conf);

    // hs_init(NULL, NULL);	
    
    // Causes annoying "too may hs_exits" errors when we have this, though it's supposedly supposed to be here...
	// atexit(&hs_exit);
// }

@end

