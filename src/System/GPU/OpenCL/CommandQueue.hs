-- -----------------------------------------------------------------------------
-- This file is part of Haskell-Opencl.

-- Haskell-Opencl is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- Haskell-Opencl is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Haskell-Opencl.  If not, see <http://www.gnu.org/licenses/>.
-- -----------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module System.GPU.OpenCL.CommandQueue(
  -- * Types
  CLCommandQueue, CLCommandQueueProperty(..), 
  -- * Command Queue Functions
  clCreateCommandQueue, clRetainCommandQueue, clReleaseCommandQueue,
  clGetCommandQueueContext, clGetCommandQueueDevice, 
  clGetCommandQueueReferenceCount, clGetCommandQueueProperties,
  clSetCommandQueueProperty
  -- * Memory Commands
  -- * Executing Kernels
  -- * Flush and Finish
                                     ) where

-- -----------------------------------------------------------------------------
import Foreign( Ptr, castPtr, nullPtr, alloca, peek )
import Foreign.C.Types( CSize, CInt, CUInt, CULong )
import Foreign.Storable( sizeOf )
import Foreign.Marshal.Utils( fromBool )
import System.GPU.OpenCL.Types( 
  CLCommandQueue, CLDeviceID, CLContext, CLCommandQueueProperty(..),
  bitmaskToCommandQueueProperties, bitmaskFromCommandQueueProperties )
import System.GPU.OpenCL.Errors( ErrorCode(..), clSuccess )

-- -----------------------------------------------------------------------------
foreign import ccall "clCreateCommandQueue" raw_clCreateCommandQueue :: 
  CLContext -> CLDeviceID -> CULong -> Ptr CInt -> IO CLCommandQueue
foreign import ccall "clRetainCommandQueue" raw_clRetainCommandQueue :: 
  CLCommandQueue -> IO CInt
foreign import ccall "clReleaseCommandQueue" raw_clReleaseCommandQueue :: 
  CLCommandQueue -> IO CInt
foreign import ccall "clGetCommandQueueInfo" raw_clGetCommandQueueInfo :: 
  CLCommandQueue -> CUInt -> CSize -> Ptr () -> Ptr CSize -> IO CInt
foreign import ccall "clSetCommandQueueProperty" raw_clSetCommandQueueProperty :: 
  CLCommandQueue -> CULong -> CUInt -> Ptr CULong -> IO CInt

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
                     -> IO (Maybe CLCommandQueue)
clCreateCommandQueue ctx did xs = alloca $ \perr -> do
  cq <- raw_clCreateCommandQueue ctx did props perr
  errcode <- peek perr >>= return . ErrorCode
  if errcode == clSuccess
    then return . Just $ cq
    else return Nothing
    where
      props = bitmaskFromCommandQueueProperties xs

-- | Increments the command_queue reference count.
-- 'clCreateCommandQueue' performs an implicit retain. This is very helpful for 
-- 3rd party libraries, which typically get a command-queue passed to them by 
-- the application. However, it is possible that the application may delete the 
-- command-queue without informing the library. Allowing functions to attach to 
-- (i.e. retain) and release a command-queue solves the problem of a 
-- command-queue being used by a library no longer being valid.
-- Returns 'True' if the function is executed successfully. It returns 'False'
-- if command_queue is not a valid command-queue.
clRetainCommandQueue :: CLCommandQueue -> IO Bool
clRetainCommandQueue cq = raw_clRetainCommandQueue cq
                          >>= return . (==clSuccess) . ErrorCode

-- | Decrements the command_queue reference count.
-- After the command_queue reference count becomes zero and all commands queued 
-- to command_queue have finished (e.g., kernel executions, memory object 
-- updates, etc.), the command-queue is deleted.
-- Returns 'True' if the function is executed successfully. It returns 'False'
-- if command_queue is not a valid command-queue.
clReleaseCommandQueue :: CLCommandQueue -> IO Bool
clReleaseCommandQueue cq = raw_clReleaseCommandQueue cq
                       >>= return . (==clSuccess) . ErrorCode

-- | Return the context specified when the command-queue is created.
clGetCommandQueueContext :: CLCommandQueue -> IO (Maybe CLContext)
clGetCommandQueueContext cq = alloca $ \(dat :: Ptr CLContext) -> do
  errcode <- fmap ErrorCode $ raw_clGetCommandQueueInfo cq 0x1090 size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (nullPtr::CLContext)

-- | Return the device specified when the command-queue is created.
clGetCommandQueueDevice :: CLCommandQueue -> IO (Maybe CLDeviceID)
clGetCommandQueueDevice cq = alloca $ \(dat :: Ptr CLDeviceID) -> do
  errcode <- fmap ErrorCode $ raw_clGetCommandQueueInfo cq 0x1091 size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (nullPtr::CLDeviceID)

-- | Return the command-queue reference count.
-- The reference count returned should be considered immediately stale. It is 
-- unsuitable for general use in applications. This feature is provided for 
-- identifying memory leaks.
clGetCommandQueueReferenceCount :: CLCommandQueue -> IO (Maybe CUInt)
clGetCommandQueueReferenceCount cq = alloca $ \(dat :: Ptr CUInt) -> do
  errcode <- fmap ErrorCode $ raw_clGetCommandQueueInfo cq 0x1092 size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CUInt)


-- | Return the currently specified properties for the command-queue. These 
-- properties are specified by the properties argument in 'clCreateCommandQueue'
-- , and can be changed by 'clSetCommandQueueProperty'.
clGetCommandQueueProperties :: CLCommandQueue -> IO [CLCommandQueueProperty]
clGetCommandQueueProperties cq = alloca $ \(dat :: Ptr CULong) -> do
  errcode <- fmap ErrorCode $ raw_clGetCommandQueueInfo cq 0x1093 size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap bitmaskToCommandQueueProperties $ peek dat
    else return []
    where 
      size = fromIntegral $ sizeOf (0::CULong)

-- | Enable or disable the properties of a command-queue.
-- Returns the command-queue properties before they were changed by 
-- 'clSetCommandQueueProperty'.
-- As specified for 'clCreateCommandQueue', the 
-- 'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE' command-queue property determines 
-- whether the commands in a command-queue are executed in-order or 
-- out-of-order. Changing this command-queue property will cause the OpenCL 
-- implementation to block until all previously queued commands in command_queue 
-- have completed. This can be an expensive operation and therefore changes to 
-- the 'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE' property should be only done 
-- when absolutely necessary.
-- 
-- It is possible that a device(s) becomes unavailable after a context and 
-- command-queues that use this device(s) have been created and commands have 
-- been queued to command-queues. In this case the behavior of OpenCL API calls 
-- that use this context (and command-queues) are considered to be 
-- implementation-defined. The user callback function, if specified when the 
-- context is created, can be used to record appropriate information 
-- when the device becomes unavailable.
clSetCommandQueueProperty :: CLCommandQueue -> [CLCommandQueueProperty] -> Bool 
                          -> IO [CLCommandQueueProperty]
clSetCommandQueueProperty cq xs val = alloca $ \(dat :: Ptr CULong) -> do
  errcode <- fmap ErrorCode $ raw_clSetCommandQueueProperty cq props (fromBool val) dat
  if errcode == clSuccess
    then fmap bitmaskToCommandQueueProperties $ peek dat
    else return []
    where
      props = bitmaskFromCommandQueueProperties xs

-- -----------------------------------------------------------------------------
