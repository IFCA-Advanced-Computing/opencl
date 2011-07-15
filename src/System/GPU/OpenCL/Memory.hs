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
  clCreateBuffer
  ) where

-- -----------------------------------------------------------------------------
import Foreign( Ptr )
import Foreign.C.Types( CSize )
import System.GPU.OpenCL.Types( 
  CLMem, CLContext, CLuint, CLint, CLMemFlags_, CLImageFormat_p, CLError(..),
  CLMemObjectType_, CLMemInfo_, CLImageInfo_, CLMemFlag(..) )

-- -----------------------------------------------------------------------------
foreign import ccall "clCreateBuffer" raw_clCreateBuffer :: 
  CLContext -> CLMemFlags_ -> CSize -> Ptr () -> Ptr CLint -> IO CLMem
foreign import ccall "clCreateImage2D" raw_clCreateImage2D :: 
  CLContext -> CLMemFlags_ -> CLImageFormat_p -> CSize -> CSize -> CSize 
  -> Ptr () -> Ptr CLint -> IO CLMem
foreign import ccall "clCreateImage3D" raw_clCreateImage3D :: 
  CLContext -> CLMemFlags_-> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize 
  -> CSize -> Ptr () -> Ptr CLint -> IO CLMem
foreign import ccall "clRetainMemObject" raw_clRetainMemObject :: 
  CLMem -> IO CLint
foreign import ccall "clReleaseMemObject" raw_clReleaseMemObject :: 
  CLMem -> IO CLint
foreign import ccall "clGetSupportedImageFormats" raw_clGetSupportedImageFormats :: 
  CLContext -> CLMemFlags_ -> CLMemObjectType_ -> CLuint -> CLImageFormat_p 
  -> Ptr CLuint -> IO CLint
foreign import ccall "clGetMemObjectInfo" raw_clGetMemObjectInfo :: 
  CLMem -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import ccall "clGetImageInfo" raw_clGetImageInfo :: 
  CLMem -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

-- -----------------------------------------------------------------------------
clCreateBuffer :: CLContext -> [CLMemFlag] -> (CSize, Ptr ()) -> IO (Either CLError CLMem)
clCreateBuffer ctx xs (sbuff,buff) = return $ Left CLSUCCESS

-- -----------------------------------------------------------------------------
