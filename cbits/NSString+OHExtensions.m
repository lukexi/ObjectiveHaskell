//
//  NSString+OHExtensions.m
//  ObjectiveHaskellMini
//
//  Created by Justin Spahr-Summers on 08.08.12.
//  Copyright (c) 2013 Justin Spahr-Summers. All rights reserved.
//

#import "NSString+OHExtensions.h"
#import "NSString_stub.h"

@implementation NSString (OHExtensions)

+ (instancetype)objectWithHaskellPointer:(OHHaskellPtr)haskellPointer {
	return OHNSStringFromHaskellPtr(haskellPointer);
}

- (OHHaskellPtr)haskellPointer {
	return OHHaskellPtrFromNSString(self);
}

@end
