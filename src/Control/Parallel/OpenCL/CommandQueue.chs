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
module Control.Parallel.OpenCL.CommandQueue(
  -- * Types
  CLCommandQueue, CLCommandQueueProperty(..), CLMapFlag(..),
  -- * Command Queue Functions
  clCreateCommandQueue, clRetainCommandQueue, clReleaseCommandQueue,
  clGetCommandQueueContext, clGetCommandQueueDevice, 
  clGetCommandQueueReferenceCount, clGetCommandQueueProperties,
  clSetCommandQueueProperty,
  -- * Memory Commands
  clEnqueueReadBuffer, clEnqueueWriteBuffer, clEnqueueReadImage, 
  clEnqueueWriteImage, clEnqueueCopyImage, clEnqueueCopyImageToBuffer,
  clEnqueueCopyBufferToImage, clEnqueueMapBuffer, clEnqueueMapImage,
  clEnqueueUnmapMemObject,
  -- * Executing Kernels
  clEnqueueNDRangeKernel, clEnqueueTask, clEnqueueNativeKernel, 
  clEnqueueMarker, clEnqueueWaitForEvents, clEnqueueBarrier,
  -- * Flush and Finish
  clFlush, clFinish
  ) where

-- -----------------------------------------------------------------------------
import Foreign
import Foreign.C.Types
import Control.Parallel.OpenCL.Types( 
  CLint, CLbool, CLuint, CLCommandQueueProperty_, CLCommandQueueInfo_, 
  CLMapFlags_, CLMapFlag(..), CLCommandQueue, CLDeviceID, CLContext, 
  CLCommandQueueProperty(..), CLEvent, CLMem, CLKernel,
  whenSuccess, wrapCheckSuccess, wrapPError, wrapGetInfo, getCLValue, 
  bitmaskToCommandQueueProperties, bitmaskFromFlags )

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

-- -----------------------------------------------------------------------------
type NativeKernelCallback = Ptr () -> IO ()
foreign import CALLCONV "wrapper" wrapNativeKernelCallback :: 
  NativeKernelCallback -> IO (FunPtr NativeKernelCallback)
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
foreign import CALLCONV "clEnqueueReadImage" raw_clEnqueueReadImage ::
  CLCommandQueue -> CLMem -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueWriteImage" raw_clEnqueueWriteImage ::
  CLCommandQueue -> CLMem -> CLbool -> Ptr CSize -> Ptr CSize -> CSize -> CSize -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueCopyImage" raw_clEnqueueCopyImage ::
  CLCommandQueue -> CLMem -> CLMem -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueCopyImageToBuffer" raw_clEnqueueCopyImageToBuffer ::
  CLCommandQueue -> CLMem -> CLMem -> Ptr CSize -> Ptr CSize -> CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueCopyBufferToImage" raw_clEnqueueCopyBufferToImage ::
  CLCommandQueue -> CLMem -> CLMem -> CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueMapBuffer" raw_clEnqueueMapBuffer ::
  CLCommandQueue -> CLMem -> CLbool -> CLMapFlags_ -> CSize -> CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> Ptr CLint -> IO (Ptr ())
foreign import CALLCONV "clEnqueueMapImage" raw_clEnqueueMapImage ::
  CLCommandQueue -> CLMem -> CLbool -> CLMapFlags_ -> Ptr CSize -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> Ptr CLint -> IO (Ptr ())
foreign import CALLCONV "clEnqueueUnmapMemObject" raw_clEnqueueUnmapMemObject ::
  CLCommandQueue -> CLMem -> Ptr () -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueNDRangeKernel" raw_clEnqueueNDRangeKernel :: 
  CLCommandQueue -> CLKernel -> CLuint -> Ptr CSize -> Ptr CSize -> Ptr CSize -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clEnqueueNativeKernel" raw_clEnqueueNativeKernel ::
  CLCommandQueue ->  FunPtr NativeKernelCallback -> Ptr () -> CSize -> CLuint -> Ptr CLMem -> Ptr (Ptr ()) -> CLuint -> Ptr CLEvent -> Ptr CLEvent -> IO CLint
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
withMaybeArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withMaybeArray [] = ($ nullPtr)
withMaybeArray xs = withArray xs

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
clGetCommandQueueContext cq =
    wrapGetInfo (\(dat :: Ptr CLContext) ->
        raw_clGetCommandQueueInfo cq infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_QUEUE_CONTEXT
      size = fromIntegral $ sizeOf (nullPtr::CLContext)

-- | Return the device specified when the command-queue is created.
--
-- This function execute OpenCL clGetCommandQueueInfo with 'CL_QUEUE_DEVICE'.
clGetCommandQueueDevice :: CLCommandQueue -> IO CLDeviceID
clGetCommandQueueDevice cq =
    wrapGetInfo (\(dat :: Ptr CLDeviceID) ->
        raw_clGetCommandQueueInfo cq infoid size (castPtr dat)) id
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
clGetCommandQueueReferenceCount cq =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        raw_clGetCommandQueueInfo cq infoid size (castPtr dat)) id
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
clGetCommandQueueProperties cq =
    wrapGetInfo (\(dat :: Ptr CLCommandQueueProperty_) ->
        raw_clGetCommandQueueInfo cq infoid size (castPtr dat)) bitmaskToCommandQueueProperties
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

{-| Enqueues a command to read from a 2D or 3D image object to host memory.

Returns an event object that identifies this particular read command and can be
used to query or queue a wait for this particular command to complete. event can
be NULL in which case it will not be possible for the application to query the
status of this command or queue a wait for this command to complete.

Notes

If blocking is 'True' i.e. the read command is blocking, 'clEnqueueReadImage'
does not return until the buffer data has been read and copied into memory
pointed to by ptr.

If blocking_read is 'False' i.e. map operation is non-blocking,
'clEnqueueReadImage' queues a non-blocking read command and returns. The
contents of the buffer that ptr points to cannot be used until the read command
has completed. The event argument returns an event object which can be used to
query the execution status of the read command. When the read command has
completed, the contents of the buffer that ptr points to can be used by the
application.

Calling 'clEnqueueReadImage' to read a region of the image object with the ptr
argument value set to host_ptr + (origin.z * image slice pitch + origin.y *
image row pitch + origin.x * bytes per pixel), where host_ptr is a pointer to
the memory region specified when the image object being read is created with
'CL_MEM_USE_HOST_PTR', must meet the following requirements in order to avoid
undefined behavior:

 * All commands that use this image object have finished execution before the
read command begins execution.

 * The row_pitch and slice_pitch argument values in clEnqueueReadImage must be
set to the image row pitch and slice pitch.

 * The image object is not mapped.

 * The image object is not used by any command-queue until the read command has
finished execution.

'clEnqueueReadImage' returns the 'CLEvent' if the function is executed
successfully. It can throw the following 'CLError' exceptions:

 * 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue.

 * 'CL_INVALID_CONTEXT' if the context associated with command_queue and image
are not the same or if the context associated with command_queue and events in
event_wait_list are not the same.

 * 'CL_INVALID_MEM_OBJECT' if image is not a valid image object.

 * 'CL_INVALID_VALUE' if the region being read specified by origin and region is
out of bounds or if ptr is a nullPtr value.

 * 'CL_INVALID_VALUE' if image is a 2D image object and z is not equal to 0 or
depth is not equal to 1 or slice_pitch is not equal to 0.

 * 'CL_INVALID_EVENT_WAIT_LIST' if event objects in event_wait_list are not
valid events.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for data store associated with image.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.

-}
clEnqueueReadImage :: Integral a 
                      => CLCommandQueue -- ^ Refers to the command-queue in
                                        -- which the read command will be
                                        -- queued. command_queue and image must
                                        -- be created with the same OpenCL
                                        -- contex
                      -> CLMem -- ^ Refers to a valid 2D or 3D image object.
                      -> Bool -- ^ Indicates if the read operations are blocking
                              -- or non-blocking.
                      -> (a,a,a) -- ^ Defines the (x, y, z) offset in pixels in
                                 -- the image from where to read. If image is a
                                 -- 2D image object, the z value given must be
                                 -- 0.
                      -> (a,a,a) -- ^ Defines the (width, height, depth) in
                                 -- pixels of the 2D or 3D rectangle being
                                 -- read. If image is a 2D image object, the
                                 -- depth value given must be 1.
                      -> a -- ^ The length of each row in bytes. This value must
                           -- be greater than or equal to the element size in
                           -- bytes * width. If row_pitch is set to 0, the
                           -- appropriate row pitch is calculated based on the
                           -- size of each element in bytes multiplied by width.
                      -> a -- ^ Size in bytes of the 2D slice of the 3D region
                           -- of a 3D image being read. This must be 0 if image
                           -- is a 2D image. This value must be greater than or
                           -- equal to row_pitch * height. If slice_pitch is set
                           -- to 0, the appropriate slice pitch is calculated
                           -- based on the row_pitch * height.
                      -> Ptr () -- ^ The pointer to a buffer in host memory
                                -- where image data is to be read from.
                      -> [CLEvent] -- ^ Specify events that need to complete
                                   -- before this particular command can be
                                   -- executed. If event_wait_list is empty,
                                   -- then this particular command does not wait
                                   -- on any event to complete. The events
                                   -- specified in the list act as
                                   -- synchronization points. The context
                                   -- associated with events in event_wait_list
                                   -- and command_queue must be the same.
                      -> IO CLEvent
clEnqueueReadImage cq mem check (orix,oriy,oriz) (regx,regy,regz) rp sp dat xs = 
  withArray (fmap fromIntegral [orix,oriy,oriz]) $ \pori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  clEnqueue (raw_clEnqueueReadImage cq mem (fromBool check) pori preg (fromIntegral rp) (fromIntegral sp) dat) xs
                       
{-| Enqueues a command to write from a 2D or 3D image object to host memory.

Returns an event object that identifies this particular write command and can be
used to query or queue a wait for this particular command to complete. event can
be NULL in which case it will not be possible for the application to query the
status of this command or queue a wait for this command to complete.

Notes

If blocking_write is 'True' the OpenCL implementation copies the data referred
to by ptr and enqueues the write command in the command-queue. The memory
pointed to by ptr can be reused by the application after the
'clEnqueueWriteImage' call returns.

If blocking_write is 'False' the OpenCL implementation will use ptr to perform a
nonblocking write. As the write is non-blocking the implementation can return
immediately. The memory pointed to by ptr cannot be reused by the application
after the call returns. The event argument returns an event object which can be
used to query the execution status of the write command. When the write command
has completed, the memory pointed to by ptr can then be reused by the
application.

Calling 'clEnqueueWriteImage' to update the latest bits in a region of the image
object with the ptr argument value set to host_ptr + (origin.z * image slice
pitch + origin.y * image row pitch + origin.x * bytes per pixel), where host_ptr
is a pointer to the memory region specified when the image object being written
is created with 'CL_MEM_USE_HOST_PTR', must meet the following requirements in
order to avoid undefined behavior:

 * The host memory region being written contains the latest bits when the
enqueued write command begins execution.

 * The input_row_pitch and input_slice_pitch argument values in
clEnqueueWriteImage must be set to the image row pitch and slice pitch.

 * The image object is not mapped.

 * The image object is not used by any command-queue until the write command has
finished execution.

'clEnqueueWriteImage' returns the 'CLEvent' if the function is executed
successfully. It can throw the following 'CLError' exceptions:

 * 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue.

 * 'CL_INVALID_CONTEXT' if the context associated with command_queue and image
are not the same or if the context associated with command_queue and events in
event_wait_list are not the same.

 * 'CL_INVALID_MEM_OBJECT' if image is not a valid image object.

 * 'CL_INVALID_VALUE' if the region being write or written specified by origin
and region is out of bounds or if ptr is a NULL value.

 * 'CL_INVALID_VALUE' if image is a 2D image object and z is not equal to 0 or
depth is not equal to 1 or slice_pitch is not equal to 0.

 * 'CL_INVALID_EVENT_WAIT_LIST' if event objects in event_wait_list are not
valid events.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for data store associated with image.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.

-}
clEnqueueWriteImage :: Integral a 
                       => CLCommandQueue -- ^ Refers to the command-queue in
                                         -- which the write command will be
                                         -- queued. command_queue and image must
                                         -- be created with the same OpenCL
                                         -- contex
                       -> CLMem -- ^ Refers to a valid 2D or 3D image object.
                       -> Bool -- ^ Indicates if the write operation is blocking
                               -- or non-blocking.
                       -> (a,a,a) -- ^ Defines the (x, y, z) offset in pixels in
                                  -- the image from where to write or write. If
                                  -- image is a 2D image object, the z value
                                  -- given must be 0.
                       -> (a,a,a) -- ^ Defines the (width, height, depth) in
                                  -- pixels of the 2D or 3D rectangle being
                                  -- write or written. If image is a 2D image
                                  -- object, the depth value given must be 1.
                       -> a -- ^ The length of each row in bytes. This value
                            -- must be greater than or equal to the element size
                            -- in bytes * width. If input_row_pitch is set to 0,
                            -- the appropriate row pitch is calculated based on
                            -- the size of each element in bytes multiplied by
                            -- width.
                       -> a -- ^ Size in bytes of the 2D slice of the 3D region
                            -- of a 3D image being written. This must be 0 if
                            -- image is a 2D image. This value must be greater
                            -- than or equal to row_pitch * height. If
                            -- input_slice_pitch is set to 0, the appropriate
                            -- slice pitch is calculated based on the row_pitch
                            -- * height.
                       -> Ptr () -- ^ The pointer to a buffer in host memory
                                 -- where image data is to be written to.
                       -> [CLEvent] -- ^ Specify events that need to complete
                                    -- before this particular command can be
                                    -- executed. If event_wait_list is empty,
                                    -- then this particular command does not
                                    -- wait on any event to complete. The events
                                    -- specified in event_wait_list act as
                                    -- synchronization points. The context
                                    -- associated with events in event_wait_list
                                    -- and command_queue must be the same.
                       -> IO CLEvent
clEnqueueWriteImage cq mem check (orix,oriy,oriz) (regx,regy,regz) rp sp dat xs = 
  withArray (fmap fromIntegral [orix,oriy,oriz]) $ \pori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  clEnqueue (raw_clEnqueueWriteImage cq mem (fromBool check) pori preg (fromIntegral rp) (fromIntegral sp) dat) xs
                       
{-| Enqueues a command to copy image objects.

Notes 

It is currently a requirement that the src_image and dst_image image memory
objects for 'clEnqueueCopyImage' must have the exact same image format (i.e. the
'CLImageFormat' descriptor specified when src_image and dst_image are created
must match).

src_image and dst_image can be 2D or 3D image objects allowing us to perform the
following actions:

 * Copy a 2D image object to a 2D image object.

 * Copy a 2D image object to a 2D slice of a 3D image object.

 * Copy a 2D slice of a 3D image object to a 2D image object.

 * Copy a 3D image object to a 3D image object.

'clEnqueueCopyImage' returns the 'CLEvent' if the function is executed
successfully. It can throw the following 'CLError' exceptions:

 * 'CL_INVALID_COMMAND_QUEUE if command_queue is not a valid command-queue.

 * 'CL_INVALID_CONTEXT if the context associated with command_queue, src_image
and dst_image are not the same or if the context associated with command_queue
and events in event_wait_list are not the same.

 * 'CL_INVALID_MEM_OBJECT if src_image and dst_image are not valid image
objects.

 * 'CL_IMAGE_FORMAT_MISMATCH if src_image and dst_image do not use the same
image format.

 * 'CL_INVALID_VALUE if the 2D or 3D rectangular region specified by src_origin
and src_origin + region refers to a region outside src_image, or if the 2D or 3D
rectangular region specified by dst_origin and dst_origin + region refers to a
region outside dst_image.

 * 'CL_INVALID_VALUE if src_image is a 2D image object and src_origin.z is not
equal to 0 or region.depth is not equal to 1.

 * 'CL_INVALID_VALUE if dst_image is a 2D image object and dst_origen.z is not
equal to 0 or region.depth is not equal to 1.

 * 'CL_INVALID_EVENT_WAIT_LIST if event objects in event_wait_list are not valid
events.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE if there is a failure to allocate memory
for data store associated with src_image or dst_image.

 * 'CL_OUT_OF_HOST_MEMORY if there is a failure to allocate resources required
by the OpenCL implementation on the host.

 * 'CL_MEM_COPY_OVERLAP if src_image and dst_image are the same image object and
the source and destination regions overlap.

-}
clEnqueueCopyImage :: Integral a 
                      => CLCommandQueue -- ^ Refers to the command-queue in
                                        -- which the copy command will be
                                        -- queued. The OpenCL context associated
                                        -- with command_queue, src_image and
                                        -- dst_image must be the same.
                      -> CLMem -- ^ src
                      -> CLMem -- ^ dst
                      -> (a,a,a) -- ^ Defines the starting (x, y, z) location in
                                 -- pixels in src_image from where to start the
                                 -- data copy. If src_image is a 2D image
                                 -- object, the z value given must be 0.
                      -> (a,a,a) -- ^ Defines the starting (x, y, z) location in
                                 -- pixels in dst_image from where to start the
                                 -- data copy. If dst_image is a 2D image
                                 -- object, the z value given must be 0.
                      -> (a,a,a) -- ^ Defines the (width, height, depth) in
                                 -- pixels of the 2D or 3D rectangle to copy. If
                                 -- src_image or dst_image is a 2D image object,
                                 -- the depth value given must be 1.
                      -> [CLEvent] -- ^ Specify events that need to complete
                                   -- before this particular command can be
                                   -- executed. If event_wait_list is empty, then
                                   -- this particular command does not wait on
                                   -- any event to complete. 
                      -> IO CLEvent
clEnqueueCopyImage cq src dst (src_orix,src_oriy,src_oriz) (dst_orix,dst_oriy,dst_oriz) (regx,regy,regz) xs =
  withArray (fmap fromIntegral [src_orix,src_oriy,src_oriz]) $ \psrc_ori -> 
  withArray (fmap fromIntegral [dst_orix,dst_oriy,dst_oriz]) $ \pdst_ori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  clEnqueue (raw_clEnqueueCopyImage cq src dst psrc_ori pdst_ori preg) xs


{-| Enqueues a command to copy an image object to a buffer object.

Returns an event object that identifies this particular copy command and can be
used to query or queue a wait for this particular command to complete. event can
be NULL in which case it will not be possible for the application to query the
status of this command or queue a wait for this command to
complete. 'clEnqueueBarrier' can be used instead.

'clEnqueueCopyImageToBuffer' returns the 'CLEvent' if the function is executed
successfully. It can throw the following 'CLError' exceptions:

 * CL_INVALID_COMMAND_QUEUE if command_queue is not a valid command-queue.

 * CL_INVALID_CONTEXT if the context associated with command_queue, src_image
and dst_buffer are not the same or if the context associated with command_queue
and events in event_wait_list are not the same.

 * CL_INVALID_MEM_OBJECT if src_image is not a valid image object and dst_buffer
is not a valid buffer object.

 * CL_INVALID_VALUE if the 2D or 3D rectangular region specified by src_origin
and src_origin + region refers to a region outside src_image, or if the region
specified by dst_offset and dst_offset + dst_cb refers to a region outside
dst_buffer.

 * CL_INVALID_VALUE if src_image is a 2D image object and src_origin.z is not
equal to 0 or region.depth is not equal to 1.

 * CL_INVALID_EVENT_WAIT_LIST if event objects in event_wait_list are not valid
events.

 * CL_MEM_OBJECT_ALLOCATION_FAILURE if there is a failure to allocate memory for
data store associated with src_image or dst_buffer.

 * CL_OUT_OF_HOST_MEMORY if there is a failure to allocate resources required by
the OpenCL implementation on the host.

-}
clEnqueueCopyImageToBuffer :: Integral a 
                              => CLCommandQueue -- ^ The OpenCL context
                                                -- associated with
                                                -- command_queue, src_image, and
                                                -- dst_buffer must be the same.
                              -> CLMem -- ^ src. A valid image object.
                              -> CLMem -- ^ dst. A valid buffer object.
                              -> (a,a,a) -- ^ Defines the (x, y, z) offset in
                                         -- pixels in the image from where to
                                         -- copy. If src_image is a 2D image
                                         -- object, the z value given must be 0.
                              -> (a,a,a) -- ^ Defines the (width, height, depth)
                                         -- in pixels of the 2D or 3D rectangle
                                         -- to copy. If src_image is a 2D image
                                         -- object, the depth value given must
                                         -- be 1.
                              -> a -- ^ The offset where to begin copying data
                                   -- into dst_buffer. The size in bytes of the
                                   -- region to be copied referred to as dst_cb
                                   -- is computed as width * height * depth *
                                   -- bytes/image element if src_image is a 3D
                                   -- image object and is computed as width *
                                   -- height * bytes/image element if src_image
                                   -- is a 2D image object.
                              -> [CLEvent] -- ^ Specify events that need to
                                           -- complete before this particular
                                           -- command can be executed. If
                                           -- event_wait_list is empty, then
                                           -- this particular command does not
                                           -- wait on any event to complete. The
                                           -- events specified in
                                           -- event_wait_list act as
                                           -- synchronization points. The
                                           -- context associated with events in
                                           -- event_wait_list and command_queue
                                           -- must be the same.
                              -> IO CLEvent
clEnqueueCopyImageToBuffer cq src dst (src_orix,src_oriy,src_oriz) (regx,regy,regz) offset xs =
  withArray (fmap fromIntegral [src_orix,src_oriy,src_oriz]) $ \psrc_ori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  clEnqueue (raw_clEnqueueCopyImageToBuffer cq src dst psrc_ori preg (fromIntegral offset)) xs

{-| Enqueues a command to copy a buffer object to an image object.

The size in bytes of the region to be copied from src_buffer referred to as
src_cb is computed as width * height * depth * bytes/image element if dst_image
is a 3D image object and is computed as width * height * bytes/image element if
dst_image is a 2D image object.

Returns an event object that identifies this particular copy command and can be
used to query or queue a wait for this particular command to complete. event can
be NULL in which case it will not be possible for the application to query the
status of this command or queue a wait for this command to
complete. 'clEnqueueBarrier' can be used instead.

'clEnqueueCopyBufferToImage' returns the 'CLEvent' if the function is executed
successfully. It can throw the following 'CLError' exceptions:

 * 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue.

 * 'CL_INVALID_CONTEXT' if the context associated with command_queue, src_buffer
and dst_image are not the same or if the context associated with command_queue
and events in event_wait_list are not the same.

 * 'CL_INVALID_MEM_OBJECT' if src_buffer is not a valid buffer object and
dst_image is not a valid image object.

 * 'CL_INVALID_VALUE' if the 2D or 3D rectangular region specified by dst_origin
and dst_origin + region refers to a region outside dst_origin, or if the region
specified by src_offset and src_offset + src_cb refers to a region outside
src_buffer.

 * 'CL_INVALID_VALUE' if dst_image is a 2D image object and dst_origin.z is not
equal to 0 or region.depth is not equal to 1.

 * 'CL_INVALID_EVENT_WAIT_LIST' if event objects in event_wait_list are not
valid events.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for data store associated with src_buffer or dst_image.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.

-}
clEnqueueCopyBufferToImage :: Integral a 
                              => CLCommandQueue -- ^ The OpenCL context
                                                -- associated with
                                                -- command_queue, src_image, and
                                                -- dst_buffer must be the same.
                              -> CLMem -- ^ src. A valid buffer object.
                              -> CLMem -- ^ dst. A valid image object.
                              -> a -- ^ The offset where to begin copying data
                                   -- from src_buffer.
                              -> (a,a,a) -- ^ The (x, y, z) offset in pixels
                                         -- where to begin copying data to
                                         -- dst_image. If dst_image is a 2D
                                         -- image object, the z value given by
                                         -- must be 0.
                              -> (a,a,a) -- ^ Defines the (width, height, depth)
                                         -- in pixels of the 2D or 3D rectangle
                                         -- to copy. If dst_image is a 2D image
                                         -- object, the depth value given by
                                         -- must be 1.
                              -> [CLEvent] -- ^ Specify events that need to
                                           -- complete before this particular
                                           -- command can be executed. If
                                           -- event_wait_list is empty, then
                                           -- this particular command does not
                                           -- wait on any event to complete. The
                                           -- events specified in
                                           -- event_wait_list act as
                                           -- synchronization points. The
                                           -- context associated with events in
                                           -- event_wait_list and command_queue
                                           -- must be the same.
                              -> IO CLEvent
clEnqueueCopyBufferToImage cq src dst offset (dst_orix,dst_oriy,dst_oriz) (regx,regy,regz) xs =
  withArray (fmap fromIntegral [dst_orix,dst_oriy,dst_oriz]) $ \pdst_ori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  clEnqueue (raw_clEnqueueCopyBufferToImage cq src dst (fromIntegral offset) pdst_ori preg) xs

{-| Enqueues a command to map a region of the buffer object given by buffer into
the host address space and returns a pointer to this mapped region.

If blocking_map is 'True', 'clEnqueueMapBuffer' does not return until the
specified region in buffer can be mapped.

If blocking_map is 'False' i.e. map operation is non-blocking, the pointer to
the mapped region returned by 'clEnqueueMapBuffer' cannot be used until the map
command has completed. The event argument returns an event object which can be
used to query the execution status of the map command. When the map command is
completed, the application can access the contents of the mapped region using
the pointer returned by 'clEnqueueMapBuffer'.

Returns an event object that identifies this particular copy command and can be
used toquery or queue a wait for this particular command to complete. event can
be NULL in which case it will not be possible for the application to query the
status of this command or queue a wait for this command to complete.

The contents of the regions of a memory object mapped for writing
(i.e. 'CL_MAP_WRITE' is set in map_flags argument to 'clEnqueueMapBuffer' or
'clEnqueueMapImage') are considered to be undefined until this region is
unmapped. Reads and writes by a kernel executing on a device to a memory
region(s) mapped for writing are undefined.

Multiple command-queues can map a region or overlapping regions of a memory
object for reading (i.e. map_flags = 'CL_MAP_READ'). The contents of the regions
of a memory object mapped for reading can also be read by kernels executing on a
device(s). The behavior of writes by a kernel executing on a device to a mapped
region of a memory object is undefined. Mapping (and unmapping) overlapped
regions of a buffer or image memory object for writing is undefined.

The behavior of OpenCL function calls that enqueue commands that write or copy
to regions of a memory object that are mapped is undefined.

'clEnqueueMapBuffer' will return a pointer to the mapped region if the function
is executed successfully. A nullPtr pointer is returned otherwise with one of
the following exception:

 * 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue.

 * 'CL_INVALID_CONTEXT' if the context associated with command_queue, src_image
and dst_buffer are not the same or if the context associated with command_queue
and events in event_wait_list are not the same.

 * 'CL_INVALID_MEM_OBJECT' if buffer is not a valid buffer object.

 * 'CL_INVALID_VALUE' if region being mapped given by (offset, cb) is out of
bounds or if values specified in map_flags are not valid

 * 'CL_INVALID_EVENT_WAIT_LIST' if event objects in event_wait_list are not
valid events.

 * 'CL_MAP_FAILURE' if there is a failure to map the requested region into the
host address space. This error cannot occur for buffer objects created with
'CL_MEM_USE_HOST_PTR' or 'CL_MEM_ALLOC_HOST_PTR'.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for data store associated with buffer.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.

The pointer returned maps a region starting at offset and is atleast cb bytes in
size. The result of a memory access outside this region is undefined.

-}
clEnqueueMapBuffer :: Integral a => CLCommandQueue 
                      -> CLMem -- ^ A valid buffer object. The OpenCL context
                               -- associated with command_queue and buffer must
                               -- be the same.
                      -> Bool -- ^ Indicates if the map operation is blocking or
                              -- non-blocking.
                      -> [CLMapFlag] -- ^ Is a list and can be set to
                                     -- 'CL_MAP_READ' to indicate that the
                                     -- region specified by (offset, cb) in the
                                     -- buffer object is being mapped for
                                     -- reading, and/or 'CL_MAP_WRITE' to
                                     -- indicate that the region specified by
                                     -- (offset, cb) in the buffer object is
                                     -- being mapped for writing.
                      -> a -- ^ The offset in bytes of the region in the buffer
                           -- object that is being mapped.
                      -> a -- ^ The size in bytes of the region in the buffer
                           -- object that is being mapped.
                      -> [CLEvent] -- ^ Specify events that need to complete
                                   -- before this particular command can be
                                   -- executed. If event_wait_list is empty,
                                   -- then this particular command does not wait
                                   -- on any event to complete. The events
                                   -- specified in event_wait_list act as
                                   -- synchronization points. The context
                                   -- associated with events in event_wait_list
                                   -- and command_queue must be the same.

                      -> IO (CLEvent, Ptr ())
clEnqueueMapBuffer cq mem check xs offset cb [] = 
  alloca $ \pevent -> do
    val <- wrapPError $ \perr -> raw_clEnqueueMapBuffer cq mem (fromBool check) flags (fromIntegral offset) (fromIntegral cb) 0 nullPtr pevent perr
    event <- peek pevent
    return (event, val)
    
      where
        flags = bitmaskFromFlags xs
clEnqueueMapBuffer cq mem check xs offset cb events = 
  allocaArray nevents $ \pevents -> do
    pokeArray pevents events
    alloca $ \pevent -> do
      val <- wrapPError $ \perr -> raw_clEnqueueMapBuffer cq mem (fromBool check) flags (fromIntegral offset) (fromIntegral cb) cnevents pevents pevent perr
      event <- peek pevent
      return (event, val)
    where
      flags = bitmaskFromFlags xs
      nevents = length events
      cnevents = fromIntegral nevents

{-| Enqueues a command to map a region of an image object into the host address
space and returns a pointer to this mapped region.

If blocking_map is 'False' i.e. map operation is non-blocking, the pointer to
the mapped region returned by 'clEnqueueMapImage' cannot be used until the map
command has completed. The event argument returns an event object which can be
used to query the execution status of the map command. When the map command is
completed, the application can access the contents of the mapped region using
the pointer returned by 'clEnqueueMapImage'.

Returns an event object that identifies this particular copy command and can be
used to query or queue a wait for this particular command to complete. event can
be NULL in which case it will not be possible for the application to query the
status of this command or queue a wait for this command to complete.

If the buffer or image object is created with 'CL_MEM_USE_HOST_PTR' set in
mem_flags, the following will be true:

* The host_ptr specified in 'clCreateBuffer', 'clCreateImage2D', or
'clCreateImage3D' is guaranteed to contain the latest bits in the region being
mapped when the 'clEnqueueMapBuffer' or 'clEnqueueMapImage' command has
completed.

 * The pointer value returned by 'clEnqueueMapBuffer' or 'clEnqueueMapImage'
will be derived from the host_ptr specified when the buffer or image object is
created.  

The contents of the regions of a memory object mapped for writing
(i.e. 'CL_MAP_WRITE' is set in map_flags argument to 'clEnqueueMapBuffer' or
'clEnqueueMapImage') are considered to be undefined until this region is
unmapped. Reads and writes by a kernel executing on a device to a memory
region(s) mapped for writing are undefined.

Multiple command-queues can map a region or overlapping regions of a memory
object for reading (i.e. map_flags = 'CL_MAP_READ'). The contents of the regions
of a memory object mapped for reading can also be read by kernels executing on a
device(s). The behavior of writes by a kernel executing on a device to a mapped
region of a memory object is undefined. Mapping (and unmapping) overlapped
regions of a buffer or image memory object for writing is undefined.

The behavior of OpenCL function calls that enqueue commands that write or copy
to regions of a memory object that are mapped is undefined.

'clEnqueueMapImage' will return a pointer to the mapped region if the
function is executed successfully also the scan-line (row) pitch in bytes for
the mapped region and the size in bytes of each 2D slice for the mapped
region. For a 2D image, zero is returned as slice pitch. A nullPtr pointer is
returned otherwise with one of the following exception:

 * 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue.

 * 'CL_INVALID_CONTEXT' if the context associated with command_queue and image
are not the same or if the context associated with command_queue and events in
event_wait_list are not the same.

 * 'CL_INVALID_MEM_OBJECT' if image is not a valid image object.

 * 'CL_INVALID_VALUE' if region being mapped given by (origin, origin+region) is
out of bounds or if values specified in map_flags are not valid.

 * 'CL_INVALID_VALUE' if image is a 2D image object and z is not equal to 0 or
depth is not equal to 1.

 * 'CL_INVALID_EVENT_WAIT_LIST' if event objects in event_wait_list are not
valid events.

 * 'CL_MAP_FAILURE' if there is a failure to map the requested region into the
host address space. This error cannot occur for image objects created with
'CL_MEM_USE_HOST_PTR' or 'CL_MEM_ALLOC_HOST_PTR'.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for data store associated with image.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.

The pointer returned maps a 2D or 3D region starting at origin and is atleast
(image_row_pitch * y + x) pixels in size for a 2D image, and is atleast
(image_slice_pitch * z] + image_row_pitch * y + x) pixels in size for a 3D
image. The result of a memory access outside this region is undefined.

-}
clEnqueueMapImage :: Integral a => CLCommandQueue 
                     -> CLMem -- ^ A valid image object. The OpenCL context
                              -- associated with command_queue and image must be
                              -- the same.
                     -> Bool -- ^ Indicates if the map operation is blocking or
                             -- non-blocking. If blocking_map is 'True',
                             -- 'clEnqueueMapImage' does not return until the
                             -- specified region in image can be mapped.
                     -> [CLMapFlag] -- ^ Is a bit-field and can be set to
                                    -- 'CL_MAP_READ' to indicate that the region
                                    -- specified by (origin, region) in the
                                    -- image object is being mapped for reading,
                                    -- and/or 'CL_MAP_WRITE' to indicate that the
                                    -- region specified by (origin, region) in
                                    -- the image object is being mapped for
                                    -- writing.
                     -> (a,a,a) -- ^ Define the (x, y, z) offset in pixels of
                                -- the 2D or 3D rectangle region that is to be
                                -- mapped. If image is a 2D image object, the z
                                -- value given must be 0.
                     -> (a,a,a) -- ^ Define the (width, height, depth) in pixels
                                -- of the 2D or 3D rectangle region that is to
                                -- be mapped. If image is a 2D image object, the
                                -- depth value given must be 1.
                     -> [CLEvent] -- ^ Specify events that need to complete
                                  -- before 'clEnqueueMapImage' can be
                                  -- executed. If event_wait_list is empty, then
                                  -- 'clEnqueueMapImage' does not wait on any
                                  -- event to complete. The events specified in
                                  -- event_wait_list act as synchronization
                                  -- points. The context associated with events
                                  -- in event_wait_list and command_queue must
                                  -- be the same.
                     -> IO (CLEvent, (Ptr (), CSize, CSize))
clEnqueueMapImage cq mem check xs (orix,oriy,oriz) (regx,regy,regz) [] = 
  alloca $ \ppitch -> 
  alloca $ \pslice ->
  withArray (fmap fromIntegral [orix,oriy,oriz]) $ \pori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  alloca $ \pevent -> do
    val <- wrapPError $ \perr -> raw_clEnqueueMapImage cq mem (fromBool check) flags pori preg ppitch pslice 0 nullPtr pevent perr
    event <- peek pevent
    pitch <- peek ppitch
    slice <- peek pslice
    return (event, (val, pitch, slice))
    
      where
        flags = bitmaskFromFlags xs
clEnqueueMapImage cq mem check xs (orix,oriy,oriz) (regx,regy,regz) events = 
  alloca $ \ppitch -> 
  alloca $ \pslice ->
  withArray (fmap fromIntegral [orix,oriy,oriz]) $ \pori -> 
  withArray (fmap fromIntegral [regx,regy,regz]) $ \preg -> 
  allocaArray nevents $ \pevents -> do
    pokeArray pevents events
    alloca $ \pevent -> do
      val <- wrapPError $ \perr -> raw_clEnqueueMapImage cq mem (fromBool check) flags pori preg ppitch pslice cnevents pevents pevent perr
      event <- peek pevent
      pitch <- peek ppitch
      slice <- peek pslice
      return (event, (val, pitch, slice))

    where
      flags = bitmaskFromFlags xs
      nevents = length events
      cnevents = fromIntegral nevents
      
{-| Enqueues a command to unmap a previously mapped region of a memory object.

Returns an event object that identifies this particular copy command and can be
used to query or queue a wait for this particular command to complete. event can
be NULL in which case it will not be possible for the application to query the
status of this command or queue a wait for this command to
complete. 'clEnqueueBarrier' can be used instead.

Reads or writes from the host using the pointer returned by 'clEnqueueMapBuffer'
or 'clEnqueueMapImage' are considered to be complete.

'clEnqueueMapBuffer' and 'clEnqueueMapImage' increments the mapped count of the
memory object. The initial mapped count value of a memory object is
zero. Multiple calls to 'clEnqueueMapBuffer' or 'clEnqueueMapImage' on the same
memory object will increment this mapped count by appropriate number of
calls. 'clEnqueueUnmapMemObject' decrements the mapped count of the memory
object.

'clEnqueueMapBuffer' and 'clEnqueueMapImage' act as synchronization points for a
region of the memory object being mapped.

'clEnqueueUnmapMemObject' returns the 'CLEvent' if the function is executed
successfully. It can throw the following 'CLError' exceptions:

 * CL_INVALID_COMMAND_QUEUE if command_queue is not a valid command-queue.

 * CL_INVALID_MEM_OBJECT if memobj is not a valid memory object.

 * CL_INVALID_VALUE if mapped_ptr is not a valid pointer returned by
'clEnqueueMapBuffer' or 'clEnqueueMapImage' for memobj.

 * CL_INVALID_EVENT_WAIT_LIST if event objects in event_wait_list are not valid
events.

 * CL_OUT_OF_HOST_MEMORY if there is a failure to allocate resources required by
the OpenCL implementation on the host.

 * CL_INVALID_CONTEXT if the context associated with command_queue and memobj
are not the same or if the context associated with command_queue and events in
event_wait_list are not the same.
-}
clEnqueueUnmapMemObject :: CLCommandQueue 
                           -> CLMem -- ^ A valid memory object. The OpenCL
                                    -- context associated with command_queue and
                                    -- memobj must be the same.
                           -> Ptr () -- ^ The host address returned by a
                                     -- previous call to 'clEnqueueMapBuffer' or
                                     -- 'clEnqueueMapImage' for memobj.
                           -> [CLEvent] -- ^ Specify events that need to
                                        -- complete before
                                        -- 'clEnqueueUnmapMemObject' can be
                                        -- executed. If event_wait_list is
                                        -- empty, then 'clEnqueueUnmapMemObject'
                                        -- does not wait on any event to
                                        -- complete. The events specified in
                                        -- event_wait_list act as
                                        -- synchronization points. The context
                                        -- associated with events in
                                        -- event_wait_list and command_queue
                                        -- must be the same.

                           -> IO CLEvent
clEnqueueUnmapMemObject cq mem pp = clEnqueue (raw_clEnqueueUnmapMemObject cq mem pp)

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
  
{-| Enqueues a command to execute a native C/C++ function not compiled using the
OpenCL compiler. A native user function can only be executed on a command-queue
created on a device that has 'CL_EXEC_NATIVE_KERNEL' capability set in
'clGetDeviceExecutionCapabilities'.

The data pointed to by args and cb_args bytes in size will be copied and a
pointer to this copied region will be passed to user_func. The copy needs to be
done because the memory objects ('CLMem' values) that args may contain need to
be modified and replaced by appropriate pointers to global memory. When
'clEnqueueNativeKernel' returns, the memory region pointed to by args can be
reused by the application.

Returns the evens if the kernel execution was successfully queued. It can throw
the following 'CLError' exceptions:

 * 'CL_INVALID_COMMAND_QUEUE' if command_queue is not a valid command-queue.

 * 'CL_INVALID_CONTEXT' if context associated with command_queue and events in
event-wait_list are not the same.

 * 'CL_INVALID_VALUE' if args is a NULL value and cb_args is greater than 0, or
if args is a NULL value and num_mem_objects is greater than 0.

 * 'CL_INVALID_VALUE' if args is not NULL and cb_args is 0.

 * 'CL_INVALID_OPERATION' if device cannot execute the native kernel.

 * 'CL_INVALID_MEM_OBJECT' if one or more memory objects specified in mem_list
are not valid or are not buffer objects.

 * 'CL_OUT_OF_RESOURCES' if there is a failure to queue the execution instance
of kernel on the command-queue because of insufficient resources needed to
execute the kernel.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for data store associated with buffer objects specified as arguments to kernel.

 * 'CL_INVALID_EVENT_WAIT_LIST' if event objects in event_wait_list are not
valid events.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.

-}
clEnqueueNativeKernel :: CLCommandQueue -> (Ptr () -> IO ()) -> Ptr () -> CSize 
                         -> [CLMem] -> [Ptr ()] -> [CLEvent] -> IO CLEvent
clEnqueueNativeKernel cq f dat sz xs ys evs = 
  withMaybeArray xs $ \pmem -> 
  withMaybeArray ys $ \pbuff -> do
    fptr <- wrapNativeKernelCallback f
    clEnqueue (raw_clEnqueueNativeKernel cq fptr dat sz 
               (fromIntegral . length $ xs) pmem pbuff) evs
                          
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
