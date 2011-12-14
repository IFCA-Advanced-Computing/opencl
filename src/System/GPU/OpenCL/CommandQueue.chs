{- Copyright (c) 2011 Luis Cabellos,

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of  nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, CPP #-}
module System.GPU.OpenCL.CommandQueue(
  -- * Types
  CLCommandQueue, CLCommandQueueProperty(..), 
  -- * Command Queue Functions
  clCreateCommandQueue, clRetainCommandQueue, clReleaseCommandQueue,
  clGetCommandQueueContext, clGetCommandQueueDevice, 
  clGetCommandQueueReferenceCount, clGetCommandQueueProperties,
  clSetCommandQueueProperty,
  -- * Memory Commands
  clEnqueueReadBuffer, clEnqueueWriteBuffer,
  -- * Executing Kernels
  clEnqueueNDRangeKernel, clEnqueueTask, clEnqueueMarker, 
  clEnqueueWaitForEvents, clEnqueueBarrier,
  -- * Flush and Finish
  clFlush, clFinish
  ) where

-- -----------------------------------------------------------------------------
import Foreign
import Foreign.C.Types
import System.GPU.OpenCL.Types( 
  CLint, CLbool, CLuint, CLCommandQueueProperty_, CLCommandQueueInfo_, 
  CLCommandQueue, CLDeviceID, CLContext, CLCommandQueueProperty(..), 
  CLEvent, CLMem, CLKernel,
  whenSuccess, wrapCheckSuccess, wrapPError, wrapGetInfo, getCLValue, 
  bitmaskToCommandQueueProperties, bitmaskFromFlags )

#ifdef MACOSX
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

-- -----------------------------------------------------------------------------
foreign import CALLCONV "clCreateCommandQueue" raw_clCreateCommandQueue :: 
  CLContext -> CLDeviceID -> CLCommandQueueProperty_ -> Ptr CLint -> IO CLCommandQueue
foreign import CALLCONV "clRetainCommandQueue" raw_clRetainCommandQueue :: 
  CLCommandQueue -> IO CLint
foreign import CALLCONV "clReleaseCommandQueue" raw_clReleaseCommandQueue :: 
  CLCommandQueue -> IO CLint
foreign import CALLCONV "clGetCommandQueueInfo" raw_clGetCommandQueueInfo :: 
  CLCommandQueue -> CLCommandQueueInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import CALLCONV "clSetCommandQueueProperty" raw_clSetCommandQueueProperty :: 
  CLCommandQueue -> CLCommandQueueProperty_ -> CLbool -> Ptr CLCommandQueueProperty_ -> IO CLint
foreign import CALLCONV "clEnqueueReadBuffer" raw_clEnqueueReadBuffer ::
  CLCommandQueue -> CLMem -> CLbool -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueWriteBuffer" raw_clEnqueueWriteBuffer ::
  CLCommandQueue -> CLMem -> CLbool -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueNDRangeKernel" raw_clEnqueueNDRangeKernel :: 
  CLCommandQueue -> CLKernel -> CLuint -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueTask" raw_clEnqueueTask :: 
  CLCommandQueue -> CLKernel -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueMarker" raw_clEnqueueMarker :: 
  CLCommandQueue -> Ptr CLEvent -> IO CLint 
foreign import CALLCONV "clEnqueueWaitForEvents" raw_clEnqueueWaitForEvents :: 
  CLCommandQueue -> CLuint -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueBarrier" raw_clEnqueueBarrier :: 
  CLCommandQueue -> IO CLint 
foreign import CALLCONV "clFlush" raw_clFlush ::
  CLCommandQueue -> IO CLint
foreign import CALLCONV "clFinish" raw_clFinish ::
  CLCommandQueue -> IO CLint

-- -----------------------------------------------------------------------------
{-| Create a command-queue on a specific device.

The OpenCL functions that are submitted to a command-queue are enqueued in the 
order the calls are made but can be configured to execute in-order or 
out-of-order. The properties argument in clCreateCommandQueue can be used to 
specify the execution order.

If the 'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE' property of a command-queue is 
not set, the commands enqueued to a command-queue execute in order. For example, 
if an application calls 'clEnqueueNDRangeKernel' to execute kernel A followed by 
a 'clEnqueueNDRangeKernel' to execute kernel B, the application can assume that 
kernel A finishes first and then kernel B is executed. If the memory objects 
output by kernel A are inputs to kernel B then kernel B will see the correct 
data in memory objects produced by execution of kernel A. If the 
'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE' property of a commandqueue is set, then 
there is no guarantee that kernel A will finish before kernel B starts execution.

Applications can configure the commands enqueued to a command-queue to execute 
out-of-order by setting the 'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE' property of 
the command-queue. This can be specified when the command-queue is created or 
can be changed dynamically using 'clCreateCommandQueue'. In out-of-order 
execution mode there is no guarantee that the enqueued commands will finish 
execution in the order they were queued. As there is no guarantee that kernels 
will be executed in order, i.e. based on when the 'clEnqueueNDRangeKernel' calls 
are made within a command-queue, it is therefore possible that an earlier 
'clEnqueueNDRangeKernel' call to execute kernel A identified by event A may 
execute and/or finish later than a 'clEnqueueNDRangeKernel' call to execute 
kernel B which was called by the application at a later point in time. To 
guarantee a specific order of execution of kernels, a wait on a particular event 
(in this case event A) can be used. The wait for event A can be specified in the 
event_wait_list argument to 'clEnqueueNDRangeKernel' for kernel B.

In addition, a wait for events or a barrier command can be enqueued to the 
command-queue. The wait for events command ensures that previously enqueued 
commands identified by the list of events to wait for have finished before the 
next batch of commands is executed. The barrier command ensures that all 
previously enqueued commands in a command-queue have finished execution before 
the next batch of commands is executed.

Similarly, commands to read, write, copy or map memory objects that are enqueued 
after 'clEnqueueNDRangeKernel', 'clEnqueueTask' or 'clEnqueueNativeKernel' 
commands are not guaranteed to wait for kernels scheduled for execution to have 
completed (if the 'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE' property is set). To 
ensure correct ordering of commands, the event object returned by 
'clEnqueueNDRangeKernel', 'clEnqueueTask' or 'clEnqueueNativeKernel' can be 
used to enqueue a wait for event or a barrier command can be enqueued that must 
complete before reads or writes to the memory object(s) occur.
-}
clCreateCommandQueue :: CLContext -> CLDeviceID -> [CLCommandQueueProperty] 
                     -> IO CLCommandQueue
clCreateCommandQueue ctx did xs = wrapPError $ \perr -> do
  raw_clCreateCommandQueue ctx did props perr
    where
      props = bitmaskFromFlags xs

{-| Increments the command_queue reference count. 'clCreateCommandQueue'
performs an implicit retain. This is very helpful for 3rd party libraries, which
typically get a command-queue passed to them by the application. However, it is
possible that the application may delete the command-queue without informing the
library. Allowing functions to attach to (i.e. retain) and release a
command-queue solves the problem of a command-queue being used by a library no
longer being valid.  Returns 'True' if the function is executed successfully. It
returns 'False' if command_queue is not a valid command-queue.  
-}
clRetainCommandQueue :: CLCommandQueue -> IO Bool
clRetainCommandQueue = wrapCheckSuccess . raw_clRetainCommandQueue

-- | Decrements the command_queue reference count.
-- After the command_queue reference count becomes zero and all commands queued 
-- to command_queue have finished (e.g., kernel executions, memory object 
-- updates, etc.), the command-queue is deleted.
-- Returns 'True' if the function is executed successfully. It returns 'False'
-- if command_queue is not a valid command-queue.
clReleaseCommandQueue :: CLCommandQueue -> IO Bool
clReleaseCommandQueue = wrapCheckSuccess . raw_clReleaseCommandQueue

#c
enum CLCommandQueueInfo {
  cL_QUEUE_CONTEXT=CL_QUEUE_CONTEXT,
  cL_QUEUE_DEVICE=CL_QUEUE_DEVICE,
  cL_QUEUE_REFERENCE_COUNT=CL_QUEUE_REFERENCE_COUNT,
  cL_QUEUE_PROPERTIES=CL_QUEUE_PROPERTIES,
  };
#endc
{#enum CLCommandQueueInfo {upcaseFirstLetter} #}

-- | Return the context specified when the command-queue is created.
--
-- This function execute OpenCL clGetCommandQueueInfo with 'CL_QUEUE_CONTEXT'.
clGetCommandQueueContext :: CLCommandQueue -> IO CLContext
clGetCommandQueueContext cq = wrapGetInfo (\(dat :: Ptr CLContext) 
                                           -> raw_clGetCommandQueueInfo cq infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_QUEUE_CONTEXT
      size = fromIntegral $ sizeOf (nullPtr::CLContext)

-- | Return the device specified when the command-queue is created.
--
-- This function execute OpenCL clGetCommandQueueInfo with 'CL_QUEUE_DEVICE'.
clGetCommandQueueDevice :: CLCommandQueue -> IO CLDeviceID
clGetCommandQueueDevice cq = wrapGetInfo (\(dat :: Ptr CLDeviceID) 
                                           -> raw_clGetCommandQueueInfo cq infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_QUEUE_DEVICE
      size = fromIntegral $ sizeOf (nullPtr::CLDeviceID)

-- | Return the command-queue reference count.
-- The reference count returned should be considered immediately stale. It is 
-- unsuitable for general use in applications. This feature is provided for 
-- identifying memory leaks.
--
-- This function execute OpenCL clGetCommandQueueInfo with
-- 'CL_QUEUE_REFERENCE_COUNT'.
clGetCommandQueueReferenceCount :: CLCommandQueue -> IO CLuint
clGetCommandQueueReferenceCount cq = wrapGetInfo (\(dat :: Ptr CLuint) 
                                           -> raw_clGetCommandQueueInfo cq infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_QUEUE_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLuint)


-- | Return the currently specified properties for the command-queue. These 
-- properties are specified by the properties argument in 'clCreateCommandQueue'
-- , and can be changed by 'clSetCommandQueueProperty'.
--
-- This function execute OpenCL clGetCommandQueueInfo with
-- 'CL_QUEUE_PROPERTIES'.
clGetCommandQueueProperties :: CLCommandQueue -> IO [CLCommandQueueProperty]
clGetCommandQueueProperties cq = wrapGetInfo (\(dat :: Ptr CLCommandQueueProperty_) 
                                           -> raw_clGetCommandQueueInfo cq infoid size (castPtr dat)) bitmaskToCommandQueueProperties
    where 
      infoid = getCLValue CL_QUEUE_PROPERTIES
      size = fromIntegral $ sizeOf (0::CLCommandQueueProperty_)

{-| Enable or disable the properties of a command-queue.  Returns the
command-queue properties before they were changed by
'clSetCommandQueueProperty'.  As specified for 'clCreateCommandQueue', the
'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE' command-queue property determines
whether the commands in a command-queue are executed in-order or
out-of-order. Changing this command-queue property will cause the OpenCL
implementation to block until all previously queued commands in command_queue
have completed. This can be an expensive operation and therefore changes to the
'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE' property should be only done when
absolutely necessary.

 It is possible that a device(s) becomes unavailable after a context and
command-queues that use this device(s) have been created and commands have been
queued to command-queues. In this case the behavior of OpenCL API calls that use
this context (and command-queues) are considered to be
implementation-defined. The user callback function, if specified when the
context is created, can be used to record appropriate information when the
device becomes unavailable.
-}
clSetCommandQueueProperty :: CLCommandQueue -> [CLCommandQueueProperty] -> Bool 
                          -> IO [CLCommandQueueProperty]
clSetCommandQueueProperty cq xs val = alloca 
                                      $ \(dat :: Ptr CLCommandQueueProperty_) 
                                        -> whenSuccess (f dat)
                                           $ fmap bitmaskToCommandQueueProperties $ peek dat
    where
      f = raw_clSetCommandQueueProperty cq props (fromBool val)
      props = bitmaskFromFlags xs

-- -----------------------------------------------------------------------------
clEnqueue :: (CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint) -> [CLEvent] 
             -> IO CLEvent
clEnqueue f [] = alloca $ \event -> whenSuccess (f 0 nullPtr event)
                                    $ peek event
clEnqueue f events = allocaArray nevents $ \pevents -> do
  pokeArray pevents events
  alloca $ \event -> whenSuccess (f cnevents pevents event)
                     $ peek event
    where
      nevents = length events
      cnevents = fromIntegral nevents

-- -----------------------------------------------------------------------------
{-| Enqueue commands to read from a buffer object to host memory. Calling
clEnqueueReadBuffer to read a region of the buffer object with the ptr argument
value set to host_ptr + offset, where host_ptr is a pointer to the memory region
specified when the buffer object being read is created with
'CL_MEM_USE_HOST_PTR', must meet the following requirements in order to avoid
undefined behavior:

 * All commands that use this buffer object have finished execution before the
read command begins execution

 * The buffer object is not mapped

 * The buffer object is not used by any command-queue until the read command has
finished execution Errors

'clEnqueueReadBuffer' returns the event if the function is executed
successfully. It can throw the following 'CLError' exceptions:

 * 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue.

 * 'CL_INVALID_CONTEXT' if the context associated with command_queue and buffer
are not the same or if the context associated with command_queue and events in
event_wait_list are not the same.

 * 'CL_INVALID_MEM_OBJECT' if buffer is not a valid buffer object.

 * 'CL_INVALID_VALUE' if the region being read specified by (offset, cb) is out
of bounds or if ptr is a NULL value.

 * 'CL_INVALID_EVENT_WAIT_LIST' if event_wait_list is NULL and
num_events_in_wait_list greater than 0, or event_wait_list is not NULL and
num_events_in_wait_list is 0, or if event objects in event_wait_list are not
valid events.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for data store associated with buffer.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
clEnqueueReadBuffer :: Integral a => CLCommandQueue -> CLMem -> Bool -> a -> a
                       -> Ptr () -> [CLEvent] -> IO CLEvent
clEnqueueReadBuffer cq mem check off size dat = clEnqueue (raw_clEnqueueReadBuffer cq mem (fromBool check) (fromIntegral off) (fromIntegral size) dat)

{-| Enqueue commands to write to a buffer object from host memory.Calling
clEnqueueWriteBuffer to update the latest bits in a region of the buffer object
with the ptr argument value set to host_ptr + offset, where host_ptr is a
pointer to the memory region specified when the buffer object being written is
created with 'CL_MEM_USE_HOST_PTR', must meet the following requirements in
order to avoid undefined behavior:

 * The host memory region given by (host_ptr + offset, cb) contains the latest
bits when the enqueued write command begins execution.

 * The buffer object is not mapped.

 * The buffer object is not used by any command-queue until the write command
has finished execution.

'clEnqueueWriteBuffer' returns the Event if the function is executed
successfully. It can throw the following 'CLError' exceptions:

 * 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue.

 * 'CL_INVALID_CONTEXT' if the context associated with command_queue and buffer
are not the same or if the context associated with command_queue and events in
event_wait_list are not the same.

 * 'CL_INVALID_MEM_OBJECT' if buffer is not a valid buffer object.

 * 'CL_INVALID_VALUE' if the region being written specified by (offset, cb) is
out of bounds or if ptr is a NULL value.

 * 'CL_INVALID_EVENT_WAIT_LIST' if event_wait_list is NULL and
num_events_in_wait_list greater than 0, or event_wait_list is not NULL and
num_events_in_wait_list is 0, or if event objects in event_wait_list are not
valid events.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for data store associated with buffer.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
clEnqueueWriteBuffer :: Integral a => CLCommandQueue -> CLMem -> Bool -> a -> a
                       -> Ptr () -> [CLEvent] -> IO CLEvent
clEnqueueWriteBuffer cq mem check off size dat = clEnqueue (raw_clEnqueueWriteBuffer cq mem (fromBool check) (fromIntegral off) (fromIntegral size) dat)

-- -----------------------------------------------------------------------------
{-| Enqueues a command to execute a kernel on a device. Each work-item is
uniquely identified by a global identifier. The global ID, which can be read
inside the kernel, is computed using the value given by global_work_size and
global_work_offset. In OpenCL 1.0, the starting global ID is always (0, 0,
... 0). In addition, a work-item is also identified within a work-group by a
unique local ID. The local ID, which can also be read by the kernel, is computed
using the value given by local_work_size. The starting local ID is always (0, 0,
... 0).

Returns the event if the kernel execution was successfully queued. It can throw
the following 'CLError' exceptions:

 * 'CL_INVALID_PROGRAM_EXECUTABLE' if there is no successfully built program
executable available for device associated with command_queue.

 * 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue.

 * 'CL_INVALID_KERNEL' if kernel is not a valid kernel object.

 * 'CL_INVALID_CONTEXT' if context associated with command_queue and kernel is
not the same or if the context associated with command_queue and events in
event_wait_list are not the same.

 * 'CL_INVALID_KERNEL_ARGS' if the kernel argument values have not been
specified.

 * 'CL_INVALID_WORK_DIMENSION' if work_dim is not a valid value (i.e. a value
between 1 and 3).

 * 'CL_INVALID_WORK_GROUP_SIZE' if local_work_size is specified and number of
work-items specified by global_work_size is not evenly divisable by size of
work-group given by local_work_size or does not match the work-group size
specified for kernel using the __attribute__((reqd_work_group_size(X, Y, Z)))
qualifier in program source.

 * 'CL_INVALID_WORK_GROUP_SIZE' if local_work_size is specified and the total
number of work-items in the work-group computed as local_work_size[0]
*... local_work_size[work_dim - 1] is greater than the value specified by
'CL_DEVICE_MAX_WORK_GROUP_SIZE' in the table of OpenCL Device Queries for
clGetDeviceInfo.

 * 'CL_INVALID_WORK_GROUP_SIZE' if local_work_size is NULL and the
__attribute__((reqd_work_group_size(X, Y, Z))) qualifier is used to declare the
work-group size for kernel in the program source.

 * 'CL_INVALID_WORK_ITEM_SIZE' if the number of work-items specified in any of
local_work_size[0], ... local_work_size[work_dim - 1] is greater than the
corresponding values specified by 'CL_DEVICE_MAX_WORK_ITEM_SIZES'[0],
.... 'CL_DEVICE_MAX_WORK_ITEM_SIZES'[work_dim - 1].

 * 'CL_OUT_OF_RESOURCES' if there is a failure to queue the execution instance
of kernel on the command-queue because of insufficient resources needed to
execute the kernel. For example, the explicitly specified local_work_size causes
a failure to execute the kernel because of insufficient resources such as
registers or local memory. Another example would be the number of read-only
image args used in kernel exceed the 'CL_DEVICE_MAX_READ_IMAGE_ARGS' value for
device or the number of write-only image args used in kernel exceed the
'CL_DEVICE_MAX_WRITE_IMAGE_ARGS' value for device or the number of samplers used
in kernel exceed 'CL_DEVICE_MAX_SAMPLERS' for device.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory for data store associated with image or buffer objects specified as arguments to kernel.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required by
the OpenCL implementation on the host.
-}
clEnqueueNDRangeKernel :: Integral a => CLCommandQueue -> CLKernel -> [a] -> [a] 
                          -> [CLEvent] -> IO CLEvent
clEnqueueNDRangeKernel cq krn gws lws events = withArray (map fromIntegral gws) $ \pgws -> withMaybeArray (map fromIntegral lws) $ \plws -> do
  clEnqueue (raw_clEnqueueNDRangeKernel cq krn num nullPtr pgws plws) events
    where
      num = fromIntegral $ length gws
      withMaybeArray [] = ($ nullPtr)
      withMaybeArray xs = withArray xs

{-| Enqueues a command to execute a kernel on a device. The kernel is executed
using a single work-item.

'clEnqueueTask' is equivalent to calling 'clEnqueueNDRangeKernel' with work_dim
= 1, global_work_offset = [], global_work_size[0] set to 1, and
local_work_size[0] set to 1.

Returns the evens if the kernel execution was successfully queued. It can throw
the following 'CLError' exceptions:

 * 'CL_INVALID_PROGRAM_EXECUTABLE' if there is no successfully built program
executable available for device associated with command_queue.

 * 'CL_INVALID_COMMAND_QUEUE if' command_queue is not a valid command-queue.

 * 'CL_INVALID_KERNEL' if kernel is not a valid kernel object.

 * 'CL_INVALID_CONTEXT' if context associated with command_queue and kernel is
not the same or if the context associated with command_queue and events in
event_wait_list are not the same.

 * 'CL_INVALID_KERNEL_ARGS' if the kernel argument values have not been specified.

 * 'CL_INVALID_WORK_GROUP_SIZE' if a work-group size is specified for kernel
using the __attribute__((reqd_work_group_size(X, Y, Z))) qualifier in program
source and is not (1, 1, 1).

 * 'CL_OUT_OF_RESOURCES' if there is a failure to queue the execution instance
of kernel on the command-queue because of insufficient resources needed to
execute the kernel.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for data store associated with image or buffer objects specified as arguments to
kernel.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
clEnqueueTask :: CLCommandQueue -> CLKernel -> [CLEvent] -> IO CLEvent
clEnqueueTask cq krn = clEnqueue (raw_clEnqueueTask cq krn)
  
-- -----------------------------------------------------------------------------
-- | Enqueues a marker command to command_queue. The marker command returns an
-- event which can be used to queue a wait on this marker event i.e. wait for
-- all commands queued before the marker command to complete. Returns the event
-- if the function is successfully executed. It throw the 'CLError' exception
-- 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue and
-- throw 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources
-- required by the OpenCL implementation on the host.
clEnqueueMarker :: CLCommandQueue -> IO CLEvent
clEnqueueMarker cq = alloca $ \event 
                              -> whenSuccess (raw_clEnqueueMarker cq event)
                                 $ peek event
         
{-| Enqueues a wait for a specific event or a list of events to complete before
any future commands queued in the command-queue are executed. The context
associated with events in event_list and command_queue must be the same.

It can throw the following 'CLError' exceptions:

 * 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue.

 * 'CL_INVALID_CONTEXT' if the context associated with command_queue and events
in event_list are not the same.

 * 'CL_INVALID_VALUE' if num_events is zero.

 * 'CL_INVALID_EVENT' if event objects specified in event_list are not valid
events.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
clEnqueueWaitForEvents :: CLCommandQueue -> [CLEvent] -> IO ()
clEnqueueWaitForEvents cq [] = whenSuccess 
                               (raw_clEnqueueWaitForEvents cq 0 nullPtr)
                               $ return ()
clEnqueueWaitForEvents cq events = allocaArray nevents $ \pevents -> do
  pokeArray pevents events
  whenSuccess (raw_clEnqueueWaitForEvents cq cnevents pevents)
    $ return ()
    where
      nevents = length events
      cnevents = fromIntegral nevents

-- | 'clEnqueueBarrier' is a synchronization point that ensures that all queued
-- commands in command_queue have finished execution before the next batch of
-- commands can begin execution. It throws 'CL_INVALID_COMMAND_QUEUE' if
-- command_queue is not a valid command-queue and throws
-- 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
-- by the OpenCL implementation on the host.
clEnqueueBarrier :: CLCommandQueue -> IO ()
clEnqueueBarrier cq = whenSuccess (raw_clEnqueueBarrier cq) $ return ()
  
-- -----------------------------------------------------------------------------
{-| Issues all previously queued OpenCL commands in a command-queue to the
device associated with the command-queue.  'clFlush' only guarantees that all
queued commands to command_queue get issued to the appropriate device. There is
no guarantee that they will be complete after 'clFlush' returns.

 'clFlush' returns 'True' if the function call was executed successfully. It
returns 'False' if command_queue is not a valid command-queue or if there is a
failure to allocate resources required by the OpenCL implementation on the host.

 Any blocking commands queued in a command-queue such as 'clEnqueueReadImage' or
'clEnqueueReadBuffer' with blocking_read set to 'True', 'clEnqueueWriteImage' or
'clEnqueueWriteBuffer' with blocking_write set to 'True', 'clEnqueueMapImage' or
'clEnqueueMapBuffer' with blocking_map set to 'True' or 'clWaitForEvents'
perform an implicit flush of the command-queue.

 To use event objects that refer to commands enqueued in a command-queue as
event objects to wait on by commands enqueued in a different command-queue, the
application must call a 'clFlush' or any blocking commands that perform an
implicit flush of the command-queue where the commands that refer to these event
objects are enqueued.
-}
clFlush :: CLCommandQueue -> IO Bool
clFlush = wrapCheckSuccess . raw_clFlush
             
-- | Blocks until all previously queued OpenCL commands in a command-queue are 
-- issued to the associated device and have completed.
-- 'clFinish' does not return until all queued commands in command_queue have 
-- been processed and completed. 'clFinish' is also a synchronization point.
--
-- 'clFinish' returns 'True' if the function call was executed successfully. It 
-- returns 'False' if command_queue is not a valid command-queue or if there is 
-- a failure to allocate resources required by the OpenCL implementation on the 
-- host.
clFinish :: CLCommandQueue -> IO Bool
clFinish = wrapCheckSuccess . raw_clFinish
             
-- -----------------------------------------------------------------------------
