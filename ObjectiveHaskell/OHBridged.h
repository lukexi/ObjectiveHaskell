//
//  OHBridged.h
//  ObjectiveHaskell
//
//  Created by Justin Spahr-Summers on 08.08.12.
//  Released into the public domain.
//

#import <Foundation/Foundation.h>
#import "OHTypes.h"

/*
 * Represents any class that can be bridged to and from Haskell.
 */
@protocol OHBridged <NSObject>
@required

/*
 * Returns an object that has the value wrapped by the given Haskell pointer.
 */
+ (instancetype)objectWithHaskellPointer:(OHHaskellPtr)haskellPointer;

/*
 * Returns a Haskell pointer corresponding to the receiver.
 */
- (OHHaskellPtr)haskellPointer; 

@end
