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
module System.GPU.OpenCL.Program(  
  -- * Types
  CLProgram
  ) where

-- -----------------------------------------------------------------------------
import Foreign
import Foreign.C.Types
import Foreign.C.String( CString )
import System.GPU.OpenCL.Types( 
  CLint, CLuint, CLProgram, CLContext, CLDeviceID )

#include <CL/cl.h>

-- -----------------------------------------------------------------------------
type BuildCallback = CString -> Ptr () -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapBuildCallback :: 
  BuildCallback -> IO (FunPtr BuildCallback)
foreign import ccall "clCreateProgramWithSource" raw_clCreateProgramWithSource :: 
  CLContext -> CLuint -> Ptr CString -> Ptr CSize -> Ptr CLint -> IO CLProgram
foreign import ccall "clCreateProgramWithBinary" raw_clCreateProgramWithBinary :: 
  CLContext -> CLuint -> Ptr CLDeviceID -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CLint -> Ptr CLint -> IO CLProgram
foreign import ccall "clRetainProgram" raw_clRetainProgram :: 
  CLProgram -> IO CLint
foreign import ccall "clReleaseProgram" raw_clReleaseProgram :: 
  CLProgram -> IO CLint
foreign import ccall "clBuildProgram" raw_clBuildProgram :: 
  CLProgram -> CLuint -> Ptr CLDeviceID -> CString -> FunPtr BuildCallback -> Ptr () -> IO CLint
foreign import ccall "clUnloadCompiler" raw_clUnloadCompiler :: 
  IO CLint
foreign import ccall "clGetProgramInfo" raw_clGetProgramInfo :: 
  CLProgram -> CLuint -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import ccall "clGetProgramBuildInfo"  raw_clGetProgramBuildInfo :: 
  CLProgram -> CLuint -> CSize -> Ptr () -> Ptr CSize -> IO CLint

-- -----------------------------------------------------------------------------
