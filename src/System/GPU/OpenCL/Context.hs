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
{-# LANGUAGE ForeignFunctionInterface #-}
module System.GPU.OpenCL.Context(
  -- * Types
  CLContext,
  -- * Context Functions
  clCreateContext, clCreateContextFromType, clRetainContext, clReleaseContext )
    where

-- -----------------------------------------------------------------------------
import Foreign( 
  Ptr, FunPtr, nullPtr, alloca, allocaArray, peek, pokeArray )
import Foreign.C.Types( CSize, CInt, CUInt, CULong )
import Foreign.C.String( CString, peekCString )
import System.GPU.OpenCL.Types( 
  CLDeviceID, CLContext, CLDeviceType, bitmaskFromDeviceTypes )
import System.GPU.OpenCL.Errors( ErrorCode(..), clSuccess )

-- -----------------------------------------------------------------------------
type ContextCallback = CString -> Ptr () -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapContextCallback :: 
  ContextCallback -> IO (FunPtr ContextCallback)
foreign import ccall "clCreateContext" raw_clCreateContext ::
  Ptr (Ptr CInt) -> CUInt -> Ptr CLDeviceID -> FunPtr ContextCallback -> 
  Ptr () -> Ptr CInt -> IO CLContext
foreign import ccall "clCreateContextFromType" raw_clCreateContextFromType :: 
  Ptr (Ptr CInt) -> CULong -> FunPtr ContextCallback -> 
  Ptr () -> Ptr CInt -> IO CLContext
foreign import ccall "clRetainContext" raw_clRetainContext :: 
  CLContext -> IO CInt
foreign import ccall "clReleaseContext" raw_clReleaseContext :: 
  CLContext -> IO CInt

-- -----------------------------------------------------------------------------
mkContextCallback :: (String -> IO ()) -> ContextCallback
mkContextCallback f msg _ _ _ = peekCString msg >>= f

-- | Creates an OpenCL context.
-- An OpenCL context is created with one or more devices. Contexts are used by 
-- the OpenCL runtime for managing objects such as command-queues, memory, 
-- program and kernel objects and for executing kernels on one or more devices 
-- specified in the context.
clCreateContext :: [CLDeviceID] -> (String -> IO ()) -> IO (Maybe CLContext)
clCreateContext devs f = allocaArray ndevs $ \pdevs -> do
  pokeArray pdevs devs
  alloca $ \perr -> do
    fptr <- wrapContextCallback $ mkContextCallback f
    context <- raw_clCreateContext nullPtr cndevs pdevs fptr nullPtr perr
    errcode <- peek perr >>= return . ErrorCode
    if errcode == clSuccess
      then return . Just $ context
      else return Nothing
    where
      ndevs = length devs
      cndevs = fromIntegral ndevs

-- | Create an OpenCL context from a device type that identifies the specific 
-- device(s) to use.
clCreateContextFromType :: [CLDeviceType] -> (String -> IO ()) -> IO (Maybe CLContext)
clCreateContextFromType xs f = alloca $ \perr -> do
  fptr <- wrapContextCallback $ mkContextCallback f
  context <- raw_clCreateContextFromType nullPtr types fptr nullPtr perr
  errcode <- peek perr >>= return . ErrorCode
  if errcode == clSuccess
    then return . Just $ context
    else return Nothing
    where
      types = bitmaskFromDeviceTypes xs

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
clRetainContext ctx = raw_clRetainContext ctx 
                      >>= return . (==clSuccess) . ErrorCode

-- | Decrement the context reference count.
-- After the context reference count becomes zero and all the objects attached 
-- to context (such as memory objects, command-queues) are released, the 
-- context is deleted.
-- Returns 'True' if the function is executed successfully, or 'False' if 
-- context is not a valid OpenCL context.
clReleaseContext :: CLContext -> IO Bool
clReleaseContext ctx = raw_clReleaseContext ctx 
                       >>= return . (==clSuccess) . ErrorCode

-- -----------------------------------------------------------------------------
