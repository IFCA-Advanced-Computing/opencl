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
module Control.Parallel.OpenCL.Event(  
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
import Control.Parallel.OpenCL.Types( 
  CLEvent, CLint, CLuint, CLulong, CLEventInfo_, CLProfilingInfo_,
  CLCommandQueue, CLCommandType(..), CLCommandType_, 
  CLCommandExecutionStatus(..), CLProfilingInfo(..), getCommandExecutionStatus, 
  getCLValue, getEnumCL, wrapCheckSuccess, wrapGetInfo )

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

-- -----------------------------------------------------------------------------
foreign import CALLCONV "clWaitForEvents" raw_clWaitForEvents :: 
  CLuint -> Ptr CLEvent -> IO CLint
foreign import CALLCONV "clGetEventInfo" raw_clGetEventInfo :: 
  CLEvent -> CLEventInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import CALLCONV "clRetainEvent" raw_clRetainEvent :: 
  CLEvent -> IO CLint 
foreign import CALLCONV "clReleaseEvent" raw_clReleaseEvent :: 
  CLEvent -> IO CLint 
foreign import CALLCONV "clGetEventProfilingInfo" raw_clGetEventProfilingInfo :: 
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
  wrapCheckSuccess $ raw_clWaitForEvents (fromIntegral nevents) pevents
    where
      nevents = length xs
  
-- | Increments the event reference count.
-- The OpenCL commands that return an event perform an implicit retain.
-- Returns 'True' if the function is executed successfully. It returns 'False' 
-- if event is not a valid event object.
clRetainEvent :: CLEvent -> IO Bool
clRetainEvent ev = wrapCheckSuccess $ raw_clRetainEvent ev

-- | Decrements the event reference count.
-- Decrements the event reference count. The event object is deleted once the 
-- reference count becomes zero, the specific command identified by this event 
-- has completed (or terminated) and there are no commands in the command-queues 
-- of a context that require a wait for this event to complete.
-- Returns 'True' if the function is executed successfully. It returns 'False' 
-- if event is not a valid event object.
clReleaseEvent :: CLEvent -> IO Bool
clReleaseEvent ev = wrapCheckSuccess $ raw_clReleaseEvent ev

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
--
-- This function execute OpenCL clGetEventInfo with 'CL_EVENT_COMMAND_QUEUE'.
clGetEventCommandQueue :: CLEvent -> IO CLCommandQueue
clGetEventCommandQueue ev =
    wrapGetInfo (\(dat :: Ptr CLCommandQueue) ->
        raw_clGetEventInfo ev infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_EVENT_COMMAND_QUEUE
      size = fromIntegral $ sizeOf (nullPtr::CLCommandQueue)
      
-- | Return the command associated with event.
--
-- This function execute OpenCL clGetEventInfo with 'CL_EVENT_COMMAND_TYPE'.
clGetEventCommandType :: CLEvent -> IO CLCommandType
clGetEventCommandType ev =
    wrapGetInfo (\(dat :: Ptr CLCommandType_) ->
        raw_clGetEventInfo ev infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_EVENT_COMMAND_TYPE
      size = fromIntegral $ sizeOf (0::CLCommandType_)
      
-- | Return the event reference count. The reference count returned should be 
-- considered immediately stale. It is unsuitable for general use in applications. 
-- This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetEventInfo with 'CL_EVENT_REFERENCE_COUNT'.
clGetEventReferenceCount :: CLEvent -> IO CLint
clGetEventReferenceCount ev =
    wrapGetInfo (\(dat :: Ptr CLint) ->
        raw_clGetEventInfo ev infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_EVENT_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLint)

-- | Return the execution status of the command identified by event.
--
-- This function execute OpenCL clGetEventInfo with
-- 'CL_EVENT_COMMAND_EXECUTION_STATUS'.
clGetEventCommandExecutionStatus :: CLEvent -> IO CLCommandExecutionStatus
clGetEventCommandExecutionStatus ev =
    wrapGetInfo (\(dat :: Ptr CLint) ->
        raw_clGetEventInfo ev infoid size (castPtr dat)) getCommandExecutionStatus
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
clGetEventProfilingInfo :: CLEvent -> CLProfilingInfo -> IO CLulong
clGetEventProfilingInfo ev prof =
    wrapGetInfo (\(dat :: Ptr CLulong) ->
        raw_clGetEventProfilingInfo ev infoid size (castPtr dat)) id
    where 
      infoid = getCLValue prof
      size = fromIntegral $ sizeOf (0::CLulong)

-- -----------------------------------------------------------------------------
