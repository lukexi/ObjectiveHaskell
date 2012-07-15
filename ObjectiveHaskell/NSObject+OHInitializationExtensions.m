//
//  NSObject+OHInitializationExtensions.m
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 2012-07-13.
//  Released into the public domain.
//

#if !__IPHONE_OS_VERSION_MAX_ALLOWED
#import <crt_externs.h>
#endif

@interface NSObject (OHInitializationExtensions)
@end

/**
 * This private category ensures that the Haskell runtime is loaded at about the
 * same time as the Objective-C runtime.
 */
@implementation NSObject (OHInitializationExtensions)

+ (void)load {
#if __IPHONE_OS_VERSION_MAX_ALLOWED
    // We don't support retrieving command-line arguments on iOS
    hs_init(NULL, NULL);
#else
    hs_init(_NSGetArgc(), _NSGetArgv());
#endif

    // Shut down the Haskell runtime upon exit
    atexit(&hs_exit);
}

@end
