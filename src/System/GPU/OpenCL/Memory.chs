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
module System.GPU.OpenCL.Memory(
  -- * Types
  CLMem, CLSampler, CLMemFlag(..), CLMemObjectType(..), CLAddressingMode(..), 
  CLFilterMode(..),
  -- * Memory Functions
  clCreateBuffer, clRetainMemObject, clReleaseMemObject, clGetMemType, 
  clGetMemFlags, clGetMemSize, clGetMemHostPtr, clGetMemMapCount, 
  clGetMemReferenceCount, clGetMemContext,
  -- * Sampler Functions
  clCreateSampler, clRetainSampler, clReleaseSampler, clGetSamplerReferenceCount, 
  clGetSamplerContext, clGetSamplerAddressingMode, clGetSamplerFilterMode, 
  clGetSamplerNormalizedCoords
  ) where

-- -----------------------------------------------------------------------------
import Foreign
import Foreign.C.Types
import System.GPU.OpenCL.Types( 
  CLMem, CLContext, CLSampler, CLint, CLuint, CLbool, CLMemFlags_,
  CLMemInfo_, CLAddressingMode_, CLFilterMode_, CLSamplerInfo_, 
  CLAddressingMode(..), CLFilterMode(..), CLMemFlag(..), CLMemObjectType_, 
  CLMemObjectType(..), 
  wrapPError, wrapCheckSuccess, wrapGetInfo, getEnumCL, bitmaskFromFlags, 
  bitmaskToMemFlags, getCLValue )

#include <CL/cl.h>

-- -----------------------------------------------------------------------------
foreign import CALLCONV "clCreateBuffer" raw_clCreateBuffer :: 
  CLContext -> CLMemFlags_ -> CSize -> Ptr () -> Ptr CLint -> IO CLMem
--foreign import CALLCONV "clCreateImage2D" raw_clCreateImage2D :: 
--  CLContext -> CLMemFlags_ -> CLImageFormat_p -> CSize -> CSize -> CSize 
--  -> Ptr () -> Ptr CLint -> IO CLMem
--foreign import CALLCONV "clCreateImage3D" raw_clCreateImage3D :: 
--  CLContext -> CLMemFlags_-> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize 
--  -> CSize -> Ptr () -> Ptr CLint -> IO CLMem
foreign import CALLCONV "clRetainMemObject" raw_clRetainMemObject :: 
  CLMem -> IO CLint
foreign import CALLCONV "clReleaseMemObject" raw_clReleaseMemObject :: 
  CLMem -> IO CLint
--foreign import CALLCONV "clGetSupportedImageFormats" raw_clGetSupportedImageFormats :: 
--  CLContext -> CLMemFlags_ -> CLMemObjectType_ -> CLuint -> CLImageFormat_p 
--  -> Ptr CLuint -> IO CLint
foreign import CALLCONV "clGetMemObjectInfo" raw_clGetMemObjectInfo :: 
  CLMem -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
--foreign import CALLCONV "clGetImageInfo" raw_clGetImageInfo :: 
--  CLMem -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import CALLCONV "clCreateSampler" raw_clCreateSampler :: 
  CLContext -> CLbool -> CLAddressingMode_ -> CLFilterMode_ -> Ptr CLint -> IO CLSampler
foreign import CALLCONV "clRetainSampler" raw_clRetainSampler :: 
  CLSampler -> IO CLint
foreign import CALLCONV "clReleaseSampler" raw_clReleaseSampler :: 
  CLSampler -> IO CLint
foreign import CALLCONV "clGetSamplerInfo" raw_clGetSamplerInfo :: 
  CLSampler -> CLSamplerInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

-- -----------------------------------------------------------------------------
{-| Creates a buffer object. Returns a valid non-zero buffer object if the
buffer object is created successfully. Otherwise, it throws the 'CLError': 

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid.

 * 'CL_INVALID_BUFFER_SIZE' if size is 0 or is greater than
'clDeviceMaxMemAllocSize' value for all devices in context.

 * 'CL_INVALID_HOST_PTR' if host_ptr is NULL and 'CL_MEM_USE_HOST_PTR' or
'CL_MEM_COPY_HOST_PTR' are set in flags or if host_ptr is not NULL but
'CL_MEM_COPY_HOST_PTR' or 'CL_MEM_USE_HOST_PTR' are not set in flags.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for buffer object.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
clCreateBuffer :: Integral a => CLContext -> [CLMemFlag] -> (a, Ptr ()) -> IO CLMem
clCreateBuffer ctx xs (sbuff,buff) = wrapPError $ \perr -> do
  raw_clCreateBuffer ctx flags (fromIntegral sbuff) buff perr
    where
      flags = bitmaskFromFlags xs
    
-- | Increments the memory object reference count. returns 'True' if the
-- function is executed successfully. After the memobj reference count becomes
-- zero and commands queued for execution on a command-queue(s) that use memobj
-- have finished, the memory object is deleted. It returns 'False' if memobj is
-- not a valid memory object.
clRetainMemObject :: CLMem -> IO Bool
clRetainMemObject mem = wrapCheckSuccess $ raw_clRetainMemObject mem

-- | Decrements the memory object reference count. After the memobj reference
-- count becomes zero and commands queued for execution on a command-queue(s)
-- that use memobj have finished, the memory object is deleted. Returns 'True'
-- if the function is executed successfully. It returns 'False' if memobj is not
-- a valid memory object.
clReleaseMemObject :: CLMem -> IO Bool
clReleaseMemObject mem = wrapCheckSuccess $ raw_clReleaseMemObject mem

#c
enum CLMemInfo {
  cL_MEM_TYPE=CL_MEM_TYPE,
  cL_MEM_FLAGS=CL_MEM_FLAGS,
  cL_MEM_SIZE=CL_MEM_SIZE,
  cL_MEM_HOST_PTR=CL_MEM_HOST_PTR,
  cL_MEM_MAP_COUNT=CL_MEM_MAP_COUNT,
  cL_MEM_REFERENCE_COUNT=CL_MEM_REFERENCE_COUNT,
  cL_MEM_CONTEXT=CL_MEM_CONTEXT,
  };
#endc
{#enum CLMemInfo {upcaseFirstLetter} #}

-- | Returns the mem object type.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_TYPE'.
clGetMemType :: CLMem -> IO CLMemObjectType
clGetMemType mem =
    wrapGetInfo (\(dat :: Ptr CLMemObjectType_) ->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_MEM_TYPE
      size = fromIntegral $ sizeOf (0::CLMemObjectType_)

-- | Return the flags argument value specified when memobj was created.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_FLAGS'.
clGetMemFlags :: CLMem -> IO [CLMemFlag]
clGetMemFlags mem =
    wrapGetInfo (\(dat :: Ptr CLMemFlags_)->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) bitmaskToMemFlags
    where 
      infoid = getCLValue CL_MEM_FLAGS
      size = fromIntegral $ sizeOf (0::CLMemFlags_)

-- | Return actual size of memobj in bytes.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_SIZE'.
clGetMemSize :: CLMem -> IO CSize
clGetMemSize mem =
    wrapGetInfo (\(dat :: Ptr CSize)->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_SIZE
      size = fromIntegral $ sizeOf (0::CSize)

-- | Return the host_ptr argument value specified when memobj is created.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_HOST_PTR'.
clGetMemHostPtr :: CLMem -> IO (Ptr ())
clGetMemHostPtr mem =
    wrapGetInfo (\(dat :: Ptr (Ptr ()))->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_HOST_PTR
      size = fromIntegral $ sizeOf (nullPtr::Ptr ())

-- | Map count. The map count returned should be considered immediately
-- stale. It is unsuitable for general use in applications. This feature is
-- provided for debugging.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_MAP_COUNT'.
clGetMemMapCount :: CLMem -> IO CLuint
clGetMemMapCount mem =
    wrapGetInfo (\(dat :: Ptr CLuint)->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_MAP_COUNT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- | Return memobj reference count. The reference count returned should be
-- considered immediately stale. It is unsuitable for general use in
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_REFERENCE_COUNT'.
clGetMemReferenceCount :: CLMem -> IO CLuint
clGetMemReferenceCount mem =
    wrapGetInfo (\(dat :: Ptr CLuint)->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- | Return context specified when memory object is created.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_CONTEXT'.
clGetMemContext :: CLMem -> IO CLContext
clGetMemContext mem =
    wrapGetInfo (\(dat :: Ptr CLContext)->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_CONTEXT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- -----------------------------------------------------------------------------
{-| Creates a sampler object. A sampler object describes how to sample an image
when the image is read in the kernel. The built-in functions to read from an
image in a kernel take a sampler as an argument. The sampler arguments to the
image read function can be sampler objects created using OpenCL functions and
passed as argument values to the kernel or can be samplers declared inside a
kernel. In this section we discuss how sampler objects are created using OpenCL
functions.

Returns a valid non-zero sampler object if the sampler object is created
successfully. Otherwise, it throws one of the following 'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_INVALID_VALUE' if addressing_mode, filter_mode, or normalized_coords or a
combination of these argument values are not valid.

 * 'CL_INVALID_OPERATION' if images are not supported by any device associated
with context (i.e. 'CL_DEVICE_IMAGE_SUPPORT' specified in the table of OpenCL
Device Queries for clGetDeviceInfo is 'False').

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
clCreateSampler :: CLContext -> Bool -> CLAddressingMode -> CLFilterMode 
                   -> IO CLSampler
clCreateSampler ctx norm am fm = wrapPError $ \perr -> do
  raw_clCreateSampler ctx (fromBool norm) (getCLValue am) (getCLValue fm) perr

-- | Increments the sampler reference count. 'clCreateSampler' does an implicit
-- retain. Returns 'True' if the function is executed successfully. It returns
-- 'False' if sampler is not a valid sampler object.
clRetainSampler :: CLSampler -> IO Bool
clRetainSampler mem = wrapCheckSuccess $ raw_clRetainSampler mem

-- | Decrements the sampler reference count. The sampler object is deleted after
-- the reference count becomes zero and commands queued for execution on a
-- command-queue(s) that use sampler have finished. 'clReleaseSampler' returns
-- 'True' if the function is executed successfully. It returns 'False' if
-- sampler is not a valid sampler object.
clReleaseSampler :: CLSampler -> IO Bool
clReleaseSampler mem = wrapCheckSuccess $ raw_clReleaseSampler mem

#c
enum CLSamplerInfo {
  cL_SAMPLER_REFERENCE_COUNT=CL_SAMPLER_REFERENCE_COUNT,
  cL_SAMPLER_CONTEXT=CL_SAMPLER_CONTEXT,
  cL_SAMPLER_ADDRESSING_MODE=CL_SAMPLER_ADDRESSING_MODE,
  cL_SAMPLER_FILTER_MODE=CL_SAMPLER_FILTER_MODE,
  cL_SAMPLER_NORMALIZED_COORDS=CL_SAMPLER_NORMALIZED_COORDS
  };
#endc
{#enum CLSamplerInfo {upcaseFirstLetter} #}

-- | Return the sampler reference count. The reference count returned should be
-- considered immediately stale. It is unsuitable for general use in
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetSamplerInfo with
-- 'CL_SAMPLER_REFERENCE_COUNT'.
clGetSamplerReferenceCount :: CLSampler -> IO CLuint
clGetSamplerReferenceCount sam =
    wrapGetInfo (\(dat :: Ptr CLuint)->
        raw_clGetSamplerInfo sam infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_SAMPLER_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- | Return the context specified when the sampler is created.
--
-- This function execute OpenCL clGetSamplerInfo with 'CL_SAMPLER_CONTEXT'.
clGetSamplerContext :: CLSampler -> IO CLContext
clGetSamplerContext sam =
    wrapGetInfo (\(dat :: Ptr CLContext)->
        raw_clGetSamplerInfo sam infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_SAMPLER_CONTEXT
      size = fromIntegral $ sizeOf (nullPtr :: CLContext)

-- | Return the value specified by addressing_mode argument to clCreateSampler.
--
-- This function execute OpenCL clGetSamplerInfo with
-- 'CL_SAMPLER_ADDRESSING_MODE'.
clGetSamplerAddressingMode :: CLSampler -> IO CLAddressingMode
clGetSamplerAddressingMode sam =
    wrapGetInfo (\(dat :: Ptr CLAddressingMode_)->
        raw_clGetSamplerInfo sam infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_SAMPLER_ADDRESSING_MODE
      size = fromIntegral $ sizeOf (0 :: CLAddressingMode_)

-- | Return the value specified by filter_mode argument to clCreateSampler.
--
-- This function execute OpenCL clGetSamplerInfo with 'CL_SAMPLER_FILTER_MODE'.
clGetSamplerFilterMode :: CLSampler -> IO CLFilterMode
clGetSamplerFilterMode sam =
    wrapGetInfo (\(dat :: Ptr CLFilterMode_)->
        raw_clGetSamplerInfo sam infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_SAMPLER_FILTER_MODE
      size = fromIntegral $ sizeOf (0 :: CLFilterMode_)

-- | Return the value specified by normalized_coords argument to
-- clCreateSampler.
--
-- This function execute OpenCL clGetSamplerInfo with
-- 'CL_SAMPLER_NORMALIZED_COORDS'.
clGetSamplerNormalizedCoords :: CLSampler -> IO Bool
clGetSamplerNormalizedCoords sam =
    wrapGetInfo (\(dat :: Ptr CLbool)->
        raw_clGetSamplerInfo sam infoid size (castPtr dat)) (/=0)
    where 
      infoid = getCLValue CL_SAMPLER_NORMALIZED_COORDS
      size = fromIntegral $ sizeOf (0 :: CLbool)

-- -----------------------------------------------------------------------------
