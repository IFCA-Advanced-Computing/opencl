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
module System.GPU.OpenCL.Memory(
  -- * Types
  CLMem, CLMemFlag(..), 
  -- * Functions
  clCreateBuffer, clRetainMemObject, clReleaseMemObject
  ) where

-- -----------------------------------------------------------------------------
import Foreign( Ptr )
import Foreign.C.Types( CSize )
import System.GPU.OpenCL.Types( 
  CLMem, CLContext, CLint, CLMemFlags_, CLError, CLMemInfo_, CLImageInfo_, 
  CLMemFlag(..), wrapPError, 
  wrapCheckSuccess, bitmaskFromFlags )

-- -----------------------------------------------------------------------------
foreign import ccall "clCreateBuffer" raw_clCreateBuffer :: 
  CLContext -> CLMemFlags_ -> CSize -> Ptr () -> Ptr CLint -> IO CLMem
--foreign import ccall "clCreateImage2D" raw_clCreateImage2D :: 
--  CLContext -> CLMemFlags_ -> CLImageFormat_p -> CSize -> CSize -> CSize 
--  -> Ptr () -> Ptr CLint -> IO CLMem
--foreign import ccall "clCreateImage3D" raw_clCreateImage3D :: 
--  CLContext -> CLMemFlags_-> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize 
--  -> CSize -> Ptr () -> Ptr CLint -> IO CLMem
foreign import ccall "clRetainMemObject" raw_clRetainMemObject :: 
  CLMem -> IO CLint
foreign import ccall "clReleaseMemObject" raw_clReleaseMemObject :: 
  CLMem -> IO CLint
--foreign import ccall "clGetSupportedImageFormats" raw_clGetSupportedImageFormats :: 
--  CLContext -> CLMemFlags_ -> CLMemObjectType_ -> CLuint -> CLImageFormat_p 
--  -> Ptr CLuint -> IO CLint
foreign import ccall "clGetMemObjectInfo" raw_clGetMemObjectInfo :: 
  CLMem -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import ccall "clGetImageInfo" raw_clGetImageInfo :: 
  CLMem -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

-- -----------------------------------------------------------------------------
{-| Creates a buffer object. Returns a valid non-zero buffer object if the
buffer object is created successfully. Otherwise, it returns: 

 * 'CLINVALID_CONTEXT' if context is not a valid context.

 * 'CLINVALID_VALUE' if values specified in flags are not valid.

 * 'CLINVALID_BUFFER_SIZE' if size is 0 or is greater than
'clDeviceMaxMemAllocSize' value for all devices in context.

 * 'CLINVALID_HOST_PTR' if host_ptr is NULL and 'CLMEM_USE_HOST_PTR' or
'CLMEM_COPY_HOST_PTR' are set in flags or if host_ptr is not NULL but
'CLMEM_COPY_HOST_PTR' or 'CLMEM_USE_HOST_PTR' are not set in flags.

 * 'CLMEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for buffer object.

 * 'CLOUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
clCreateBuffer :: CLContext -> [CLMemFlag] -> (CSize, Ptr ()) -> IO (Either CLError CLMem)
clCreateBuffer ctx xs (sbuff,buff) = wrapPError $ \perr -> do
  raw_clCreateBuffer ctx flags sbuff buff perr
    where
      flags = bitmaskFromFlags xs
    
-- | Increments the memory object reference count. returns 'True' if the
-- function is executed successfully. After the memobj reference count becomes
-- zero and commands queued for execution on a command-queue(s) that use memobj
-- have finished, the memory object is deleted. It returns 'False' if memobj is
-- not a valid memory object.
clRetainMemObject :: CLMem -> IO Bool
clRetainMemObject ev = wrapCheckSuccess $ raw_clRetainMemObject ev

-- | Decrements the memory object reference count. After the memobj reference
-- count becomes zero and commands queued for execution on a command-queue(s)
-- that use memobj have finished, the memory object is deleted. Returns 'True'
-- if the function is executed successfully. It returns 'False' if memobj is not
-- a valid memory object.
clReleaseMemObject :: CLMem -> IO Bool
clReleaseMemObject ev = wrapCheckSuccess $ raw_clReleaseMemObject ev

-- -----------------------------------------------------------------------------
