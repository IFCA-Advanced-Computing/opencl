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
module System.GPU.OpenCL.Event(  
  -- * Types
  CLEvent, CLCommandType(..), CLProfilingInfo(..), CLCommandExecutionStatus(..),
  -- * Functions
  clWaitForEvents, clRetainEvent, clReleaseEvent, clGetEventCommandQueue, 
  clGetEventCommandType, clGetEventReferenceCount, 
  clGetEventCommandExecutionStatus, clGetEventProfilingInfo
  ) where

-- -----------------------------------------------------------------------------
import Foreign
import Foreign.C.Types
import System.GPU.OpenCL.Types( 
  CLEvent, CLint, CLuint, CLulong, CLEventInfo_, CLProfilingInfo_, ErrorCode(..),
  CLCommandQueue, CLCommandType(..), CLCommandType_, CLCommandExecutionStatus(..), 
  CLProfilingInfo(..), clSuccess, getCommandType, getCommandExecutionStatus, 
  getCLValue )

#include <CL/cl.h>

-- -----------------------------------------------------------------------------
foreign import ccall "clWaitForEvents" raw_clWaitForEvents :: 
  CLuint -> Ptr CLEvent -> IO CLint
foreign import ccall "clGetEventInfo" raw_clGetEventInfo :: 
  CLEvent -> CLEventInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import ccall "clRetainEvent" raw_clRetainEvent :: 
  CLEvent -> IO CLint 
foreign import ccall "clReleaseEvent" raw_clReleaseEvent :: 
  CLEvent -> IO CLint 
foreign import ccall "clGetEventProfilingInfo" raw_clGetEventProfilingInfo :: 
  CLEvent -> CLProfilingInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

-- -----------------------------------------------------------------------------
-- | Waits on the host thread for commands identified by event objects in 
-- event_list to complete. A command is considered complete if its execution 
-- status is 'CL_COMPLETE' or a negative value.
-- Returns 'True' if the function was executed successfully. It returns 'False'
-- if the list of events is empty, or if events specified in event_list do not 
-- belong to the same context, or if event objects specified in event_list are 
-- not valid event objects.
clWaitForEvents :: [CLEvent] -> IO Bool
clWaitForEvents [] = return False
clWaitForEvents xs = allocaArray nevents $ \pevents -> do
  pokeArray pevents xs
  errcode <- fmap ErrorCode $ raw_clWaitForEvents (fromIntegral nevents) pevents
  return (errcode==clSuccess) 
    where
      nevents = length xs
  
-- | Increments the event reference count.
-- The OpenCL commands that return an event perform an implicit retain.
-- Returns 'True' if the function is executed successfully. It returns 'False' 
-- if event is not a valid event object.
clRetainEvent :: CLEvent -> IO Bool
clRetainEvent ev = raw_clRetainEvent ev
                   >>= return . (==clSuccess) . ErrorCode

-- | Decrements the event reference count.
-- Decrements the event reference count. The event object is deleted once the 
-- reference count becomes zero, the specific command identified by this event 
-- has completed (or terminated) and there are no commands in the command-queues 
-- of a context that require a wait for this event to complete.
-- Returns 'True' if the function is executed successfully. It returns 'False' 
-- if event is not a valid event object.
clReleaseEvent :: CLEvent -> IO Bool
clReleaseEvent ev = raw_clReleaseEvent ev
                    >>= return . (==clSuccess) . ErrorCode

#c
enum CLEventInfo {
  cL_EVENT_COMMAND_QUEUE=CL_EVENT_COMMAND_QUEUE,
  cL_EVENT_COMMAND_TYPE=CL_EVENT_COMMAND_TYPE,
  cL_EVENT_COMMAND_EXECUTION_STATUS=CL_EVENT_COMMAND_EXECUTION_STATUS,
  cL_EVENT_REFERENCE_COUNT=CL_EVENT_REFERENCE_COUNT
  };
#endc
{#enum CLEventInfo {upcaseFirstLetter} #}


-- | Return the command-queue associated with event.
clGetEventCommandQueue :: CLEvent -> IO (Maybe CLCommandQueue)
clGetEventCommandQueue ev = alloca $ \(dat :: Ptr CLCommandQueue) -> do
  errcode <- fmap ErrorCode $ raw_clGetEventInfo ev infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      infoid = getCLValue CL_EVENT_COMMAND_QUEUE
      size = fromIntegral $ sizeOf (nullPtr::CLCommandQueue)

-- | Return the command associated with event.
clGetEventCommandType :: CLEvent -> IO (Maybe CLCommandType)
clGetEventCommandType ev = alloca $ \(dat :: Ptr CLCommandType_) -> do
  errcode <- fmap ErrorCode $ raw_clGetEventInfo ev infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap getCommandType $ peek dat
    else return Nothing
    where 
      infoid = getCLValue CL_EVENT_COMMAND_TYPE
      size = fromIntegral $ sizeOf (0::CLCommandType_)

-- | Return the event reference count. The reference count returned should be 
-- considered immediately stale. It is unsuitable for general use in applications. 
-- This feature is provided for identifying memory leaks.
clGetEventReferenceCount :: CLEvent -> IO (Maybe CLint)
clGetEventReferenceCount ev = alloca $ \(dat :: Ptr CLint) -> do
  errcode <- fmap ErrorCode $ raw_clGetEventInfo ev infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      infoid = getCLValue CL_EVENT_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLint)

-- | Return the execution status of the command identified by event.
clGetEventCommandExecutionStatus :: CLEvent -> IO (Maybe CLCommandExecutionStatus)
clGetEventCommandExecutionStatus ev = alloca $ \(dat :: Ptr CLint) -> do
  errcode <- fmap ErrorCode $ raw_clGetEventInfo ev infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap (getCommandExecutionStatus) $ peek dat
    else return Nothing
    where 
      infoid = getCLValue CL_EVENT_COMMAND_EXECUTION_STATUS
      size = fromIntegral $ sizeOf (0::CLint)
      
{-| Returns profiling information for the command associated with event if 
profiling is enabled. The unsigned 64-bit values returned can be used to measure 
the time in nano-seconds consumed by OpenCL commands.

OpenCL devices are required to correctly track time across changes in device 
frequency and power states. The 'CL_DEVICE_PROFILING_TIMER_RESOLUTION' specifies 
the resolution of the timer i.e. the number of nanoseconds elapsed before the 
timer is incremented.

Event objects can be used to capture profiling information that measure 
execution time of a command. Profiling of OpenCL commands can be enabled either 
by using a command-queue created with 'CL_QUEUE_PROFILING_ENABLE' flag set in 
properties argument to clCreateCommandQueue or by setting the 
'CL_QUEUE_PROFILING_ENABLE' flag in properties argument to 
'clSetCommandQueueProperty'.

'clGetEventProfilingInfo' returns the valueif the function is executed 
successfully and the profiling information has been recorded, and returns 
'Nothing'  if the 'CL_QUEUE_PROFILING_ENABLE' flag is not set for the 
command-queue and if the profiling information is currently not available 
(because the command identified by event has not completed), or if event is a 
not a valid event object.
-} 
clGetEventProfilingInfo :: CLEvent -> CLProfilingInfo -> IO (Maybe CLulong)
clGetEventProfilingInfo ev prof = alloca $ \(dat :: Ptr CLulong) -> do
  errcode <- fmap ErrorCode $ raw_clGetEventProfilingInfo ev infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CLulong)
      infoid = getCLValue prof

-- -----------------------------------------------------------------------------
