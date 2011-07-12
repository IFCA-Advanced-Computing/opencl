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
  clCreateContext )
       where

-- -----------------------------------------------------------------------------
import Foreign( 
  Ptr, FunPtr, nullPtr, alloca, allocaArray, peek, pokeArray )
import Foreign.C.Types( CSize, CInt, CUInt )
import Foreign.C.String( CString, peekCString )
import System.GPU.OpenCL.Types( CLDeviceID, CLContext )
import System.GPU.OpenCL.Errors( ErrorCode(..), clSuccess )

-- -----------------------------------------------------------------------------
type ContextCallback = CString -> Ptr () -> CSize -> Ptr () -> IO ()
foreign import ccall "clCreateContext" raw_clCreateContext ::
    Ptr (Ptr CInt) -> CUInt -> Ptr CLDeviceID -> FunPtr ContextCallback -> 
    Ptr () -> Ptr CInt -> IO CLContext
foreign import ccall "wrapper" wrapContextCallback :: 
    ContextCallback -> IO (FunPtr ContextCallback)

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

-- -----------------------------------------------------------------------------
