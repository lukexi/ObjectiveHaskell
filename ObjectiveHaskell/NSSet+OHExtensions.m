//
//  NSSet+OHExtensions.m
//  ObjectiveHaskell
//
//  Created by Luke Iannini on 11/18/12.
//  Copyright (c) 2012 Justin Spahr-Summers. All rights reserved.
//  Released under the MIT license.
//

#import "NSSet+OHExtensions.h"
#import "NSSet_stub.h"

@implementation NSSet (OHExtensions)

+ (instancetype)objectWithHaskellPointer:(OHHaskellPtr)haskellPointer {
	return OHNSSetFromHaskellPtr(haskellPointer);
}

- (OHHaskellPtr)haskellPointer {
	return OHHaskellPtrFromNSSet(self);
}

@end
