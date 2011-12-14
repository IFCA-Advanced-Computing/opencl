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
module System.GPU.OpenCL.Context(
  -- * Types
  CLContext,
  -- * Context Functions
  clCreateContext, clCreateContextFromType, clRetainContext, clReleaseContext,
  clGetContextReferenceCount, clGetContextDevices )
    where

-- -----------------------------------------------------------------------------
import Foreign( 
  Ptr, FunPtr, nullPtr, castPtr, alloca, allocaArray, peek, peekArray, 
  pokeArray )
import Foreign.C.Types( CSize )
import Foreign.C.String( CString, peekCString )
import Foreign.Storable( sizeOf )
import System.GPU.OpenCL.Types( 
  CLuint, CLint, CLDeviceType_, CLContextInfo_, CLContextProperty_, CLDeviceID, 
  CLContext, CLDeviceType, bitmaskFromFlags, getCLValue,
  whenSuccess, wrapCheckSuccess, wrapPError, wrapGetInfo )

#ifdef MACOSX
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

-- -----------------------------------------------------------------------------
type ContextCallback = CString -> Ptr () -> CSize -> Ptr () -> IO ()
foreign import CALLCONV "wrapper" wrapContextCallback :: 
  ContextCallback -> IO (FunPtr ContextCallback)
foreign import CALLCONV "clCreateContext" raw_clCreateContext ::
  Ptr CLContextProperty_ -> CLuint -> Ptr CLDeviceID -> FunPtr ContextCallback -> 
  Ptr () -> Ptr CLint -> IO CLContext
foreign import CALLCONV "clCreateContextFromType" raw_clCreateContextFromType :: 
  Ptr CLContextProperty_ -> CLDeviceType_ -> FunPtr ContextCallback -> 
  Ptr () -> Ptr CLint -> IO CLContext
foreign import CALLCONV "clRetainContext" raw_clRetainContext :: 
  CLContext -> IO CLint
foreign import CALLCONV "clReleaseContext" raw_clReleaseContext :: 
  CLContext -> IO CLint
foreign import CALLCONV "clGetContextInfo" raw_clGetContextInfo :: 
  CLContext -> CLContextInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

-- -----------------------------------------------------------------------------
mkContextCallback :: (String -> IO ()) -> ContextCallback
mkContextCallback f msg _ _ _ = peekCString msg >>= f

-- | Creates an OpenCL context.
-- An OpenCL context is created with one or more devices. Contexts are used by 
-- the OpenCL runtime for managing objects such as command-queues, memory, 
-- program and kernel objects and for executing kernels on one or more devices 
-- specified in the context.
clCreateContext :: [CLDeviceID] -> (String -> IO ()) -> IO CLContext
clCreateContext devs f = allocaArray ndevs $ \pdevs -> do
  pokeArray pdevs devs
  wrapPError $ \perr -> do
    fptr <- wrapContextCallback $ mkContextCallback f
    raw_clCreateContext nullPtr cndevs pdevs fptr nullPtr perr
    where
      ndevs = length devs
      cndevs = fromIntegral ndevs

-- | Create an OpenCL context from a device type that identifies the specific 
-- device(s) to use.
clCreateContextFromType :: [CLDeviceType] -> (String -> IO ()) 
                           -> IO CLContext
clCreateContextFromType xs f = wrapPError $ \perr -> do
  fptr <- wrapContextCallback $ mkContextCallback f
  raw_clCreateContextFromType nullPtr types fptr nullPtr perr
    where
      types = bitmaskFromFlags xs

-- | Increment the context reference count.
-- 'clCreateContext' and 'clCreateContextFromType' perform an implicit retain. 
-- This is very helpful for 3rd party libraries, which typically get a context 
-- passed to them by the application. However, it is possible that the 
-- application may delete the context without informing the library. Allowing 
-- functions to attach to (i.e. retain) and release a context solves the 
-- problem of a context being used by a library no longer being valid.
-- Returns 'True' if the function is executed successfully, or 'False' if 
-- context is not a valid OpenCL context.
clRetainContext :: CLContext -> IO Bool
clRetainContext ctx = wrapCheckSuccess $ raw_clRetainContext ctx 

-- | Decrement the context reference count.
-- After the context reference count becomes zero and all the objects attached 
-- to context (such as memory objects, command-queues) are released, the 
-- context is deleted.
-- Returns 'True' if the function is executed successfully, or 'False' if 
-- context is not a valid OpenCL context.
clReleaseContext :: CLContext -> IO Bool
clReleaseContext ctx = wrapCheckSuccess $ raw_clReleaseContext ctx 

getContextInfoSize :: CLContext -> CLContextInfo_ -> IO CSize
getContextInfoSize ctx infoid = alloca $ \(value_size :: Ptr CSize) -> do
  whenSuccess (raw_clGetContextInfo ctx infoid 0 nullPtr value_size)
    $ peek value_size

#c
enum CLContextInfo {
  cL_CONTEXT_REFERENCE_COUNT=CL_CONTEXT_REFERENCE_COUNT,
  cL_CONTEXT_DEVICES=CL_CONTEXT_DEVICES,
  cL_CONTEXT_PROPERTIES=CL_CONTEXT_PROPERTIES
  };
#endc
{#enum CLContextInfo {upcaseFirstLetter} #}

-- | Return the context reference count. The reference count returned should be 
-- considered immediately stale. It is unsuitable for general use in 
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetContextInfo with 'CL_CONTEXT_REFERENCE_COUNT'.
clGetContextReferenceCount :: CLContext -> IO CLuint
clGetContextReferenceCount ctx = wrapGetInfo (\(dat :: Ptr CLuint) 
                                              -> raw_clGetContextInfo ctx infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_CONTEXT_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the list of devices in context.
--
-- This function execute OpenCL clGetContextInfo with 'CL_CONTEXT_DEVICES'.
clGetContextDevices :: CLContext -> IO [CLDeviceID]
clGetContextDevices ctx = do
  size <- getContextInfoSize ctx infoid
  let n = (fromIntegral size) `div` elemSize 
    
  allocaArray n $ \(buff :: Ptr CLDeviceID) -> do
    whenSuccess (raw_clGetContextInfo ctx infoid size (castPtr buff) nullPtr)
      $ peekArray n buff
    where
      infoid = getCLValue CL_CONTEXT_DEVICES
      elemSize = sizeOf (nullPtr :: CLDeviceID)

-- -----------------------------------------------------------------------------
