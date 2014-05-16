#import "OHTypes.h"
#import "Dispatch_stub.h"

#import <Foundation/Foundation.h>

void runActionPtrOnDispatchQueue(dispatch_queue_t queue, HsStablePtr action) {
    @autoreleasepool {
        dispatch_async(queue, ^{
            HSRunAction(action);
        });
    }
}

void runActionPtrOnOperationQueue(NSOperationQueue *queue, HsStablePtr action) {
    @autoreleasepool {
        [queue addOperationWithBlock:^{
            HSRunAction(action);
        }];
    }
}