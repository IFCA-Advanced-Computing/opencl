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
module Control.Parallel.OpenCL.Context(
  -- * Types
  CLContext, CLContextProperty(..),
  -- * Context Functions
  clCreateContext, clCreateContextFromType, clRetainContext, clReleaseContext,
  clGetContextReferenceCount, clGetContextDevices, clGetContextProperties )
    where

-- -----------------------------------------------------------------------------
import Foreign( 
  Ptr, FunPtr, nullPtr, castPtr, alloca, allocaArray, peek, peekArray, 
  ptrToIntPtr, intPtrToPtr, withArray )
import Foreign.C.Types( CSize )
import Foreign.C.String( CString, peekCString )
import Foreign.Storable( sizeOf )
import Control.Parallel.OpenCL.Types( 
  CLuint, CLint, CLDeviceType_, CLContextInfo_, CLContextProperty_, CLDeviceID, 
  CLContext, CLDeviceType, CLPlatformID, bitmaskFromFlags, getCLValue, getEnumCL,
  whenSuccess, wrapCheckSuccess, wrapPError, wrapGetInfo )

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#include <CL/cl_gl.h>
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
#c
enum CLContextProperties {
  cL_CONTEXT_PLATFORM_=CL_CONTEXT_PLATFORM,
#ifdef CL_VERSION_1_1
#ifdef __APPLE__
  cL_CGL_SHAREGROUP_KHR_=CL_CONTEXT_PROPERTY_USE_CGL_SHAREGROUP_APPLE
#else
  cL_GL_CONTEXT_KHR_=CL_GL_CONTEXT_KHR,
  cL_EGL_DISPLAY_KHR_=CL_EGL_DISPLAY_KHR,
  cL_GLX_DISPLAY_KHR_=CL_GLX_DISPLAY_KHR,
  cL_WGL_HDC_KHR_=CL_WGL_HDC_KHR,
  cL_CGL_SHAREGROUP_KHR_=CL_CGL_SHAREGROUP_KHR
#endif
#endif
  };
#endc
{#enum CLContextProperties {upcaseFirstLetter} #}

-- | Specifies a context property name and its corresponding value.
data CLContextProperty = CL_CONTEXT_PLATFORM CLPlatformID 
                         -- ^ Specifies the platform to use.
#ifdef CL_VERSION_1_1
                       | CL_CGL_SHAREGROUP_KHR (Ptr ())
                         -- ^ Specifies the CGL share group to use.
#ifndef __APPLE__
                       | CL_GL_CONTEXT_KHR (Ptr ())
                       | CL_EGL_DISPLAY_KHR (Ptr ())
                       | CL_GLX_DISPLAY_KHR (Ptr ())
                       | CL_WGL_HDC_KHR (Ptr ())
#endif
#endif
                       deriving( Show )

packProperty :: CLContextProperty -> [CLContextProperty_]
packProperty (CL_CONTEXT_PLATFORM pid)   = [ getCLValue CL_CONTEXT_PLATFORM_
                                           , fromIntegral . ptrToIntPtr $ pid ]
#ifdef CL_VERSION_1_1
packProperty (CL_CGL_SHAREGROUP_KHR ptr) = [ getCLValue CL_CGL_SHAREGROUP_KHR_
                                           , fromIntegral . ptrToIntPtr $ ptr ]
#ifndef __APPLE__
packProperty (CL_GL_CONTEXT_KHR ptr)     = [ getCLValue CL_GL_CONTEXT_KHR_
                                           , fromIntegral . ptrToIntPtr $ ptr ]
packProperty (CL_EGL_DISPLAY_KHR ptr)    = [ getCLValue CL_EGL_DISPLAY_KHR_
                                           , fromIntegral . ptrToIntPtr $ ptr ]
packProperty (CL_GLX_DISPLAY_KHR ptr)    = [ getCLValue CL_GLX_DISPLAY_KHR_
                                           , fromIntegral . ptrToIntPtr $ ptr ]
packProperty (CL_WGL_HDC_KHR ptr)        = [ getCLValue CL_WGL_HDC_KHR_
                                           , fromIntegral . ptrToIntPtr $ ptr ]
#endif
#endif

packContextProperties :: [CLContextProperty] -> [CLContextProperty_]
packContextProperties [] = [0]
packContextProperties (x:xs) = packProperty x ++ packContextProperties xs

unpackContextProperties :: [CLContextProperty_] -> [CLContextProperty]
unpackContextProperties [] = error "non-exhaustive Context Property list"
unpackContextProperties [x] 
  | x == 0 = []
  | otherwise = error "non-exhaustive Context Property list"
unpackContextProperties (x:y:xs) = let ys = unpackContextProperties xs 
                                   in case getEnumCL x of
                                     CL_CONTEXT_PLATFORM_ 
                                       -> CL_CONTEXT_PLATFORM 
                                          (intPtrToPtr . fromIntegral $ y) : ys
#ifdef CL_VERSION_1_1
                                     CL_CGL_SHAREGROUP_KHR_ 
                                       -> CL_CGL_SHAREGROUP_KHR 
                                          (intPtrToPtr . fromIntegral $ y) : ys
#endif
  
-- -----------------------------------------------------------------------------
mkContextCallback :: (String -> IO ()) -> ContextCallback
mkContextCallback f msg _ _ _ = peekCString msg >>= f

-- | Creates an OpenCL context.
-- An OpenCL context is created with one or more devices. Contexts are used by 
-- the OpenCL runtime for managing objects such as command-queues, memory, 
-- program and kernel objects and for executing kernels on one or more devices 
-- specified in the context.
clCreateContext :: [CLContextProperty] -> [CLDeviceID] -> (String -> IO ()) 
                   -> IO CLContext
clCreateContext [] devs f = withArray devs $ \pdevs ->
  wrapPError $ \perr -> do
    fptr <- wrapContextCallback $ mkContextCallback f
    raw_clCreateContext nullPtr cndevs pdevs fptr nullPtr perr
    where
      cndevs = fromIntegral . length $ devs
clCreateContext props devs f = withArray devs $ \pdevs ->
  wrapPError $ \perr -> do
    fptr <- wrapContextCallback $ mkContextCallback f
    withArray (packContextProperties props) $ \pprops ->
      raw_clCreateContext pprops cndevs pdevs fptr nullPtr perr    
    where
      cndevs = fromIntegral . length $ devs

-- | Create an OpenCL context from a device type that identifies the specific 
-- device(s) to use.
clCreateContextFromType :: [CLContextProperty] -> [CLDeviceType] 
                           -> (String -> IO ()) -> IO CLContext
clCreateContextFromType [] xs f = wrapPError $ \perr -> do
  fptr <- wrapContextCallback $ mkContextCallback f
  raw_clCreateContextFromType nullPtr types fptr nullPtr perr
    where
      types = bitmaskFromFlags xs
clCreateContextFromType props xs f = wrapPError $ \perr -> do
  fptr <- wrapContextCallback $ mkContextCallback f
  withArray (packContextProperties props) $ \pprops -> 
    raw_clCreateContextFromType pprops types fptr nullPtr perr
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
clGetContextReferenceCount ctx =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        raw_clGetContextInfo ctx infoid size (castPtr dat)) id
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

clGetContextProperties :: CLContext -> IO [CLContextProperty]
clGetContextProperties ctx = do
  size <- getContextInfoSize ctx infoid
  let n = (fromIntegral size) `div` elemSize 
    
  if n == 0 
    then return []
    else allocaArray n $ \(buff :: Ptr CLContextProperty_) ->
      whenSuccess (raw_clGetContextInfo ctx infoid size (castPtr buff) nullPtr)
        $ fmap unpackContextProperties $ peekArray n buff
    where
      infoid = getCLValue CL_CONTEXT_PROPERTIES
      elemSize = sizeOf (nullPtr :: CLDeviceID)
  
-- -----------------------------------------------------------------------------
