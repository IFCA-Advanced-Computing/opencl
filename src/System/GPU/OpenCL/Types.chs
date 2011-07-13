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
module System.GPU.OpenCL.Types( 
  ErrorCode(..), CLbool, CLint, CLuint, CLulong, CLPlatformInfo_, CLMem, 
  CLProgram, CLEvent,
  CLDeviceType_, CLDeviceInfo_, CLContextInfo_, CLDeviceFPConfig(..), 
  CLDeviceMemCacheType(..), CLDeviceExecCapability(..), CLDeviceLocalMemType(..),
  CLPlatformID, CLDeviceID, CLContext, CLCommandQueue, CLContextProperty_,
  CLDeviceType(..), CLCommandQueueProperty(..), CLCommandQueueInfo_, 
  CLCommandQueueProperty_, getDeviceTypeValue, getDeviceLocalMemType, 
  getDeviceMemCacheType, bitmaskToDeviceTypes, bitmaskFromDeviceTypes, 
  bitmaskToCommandQueueProperties, bitmaskFromCommandQueueProperties, 
  bitmaskToFPConfig, bitmaskToExecCapability ) 
       where

-- -----------------------------------------------------------------------------
import Foreign( Ptr )
import Foreign.C.Types
import Data.Maybe( fromMaybe, mapMaybe )
import Data.List( foldl' )
import Data.Bits( shiftL, complement, (.|.) )
import System.GPU.OpenCL.Util( testMask )

#include <CL/cl.h>

-- -----------------------------------------------------------------------------

type CLPlatformID = {#type cl_platform_id#}
type CLDeviceID = {#type cl_device_id#}
type CLContext = {#type cl_context#}
type CLCommandQueue = {#type cl_command_queue#}
type CLMem = {#type cl_mem#}
type CLEvent = {#type cl_event#}
type CLProgram = {#type cl_program#}

type CLint = {#type cl_int#}
type CLuint = {#type cl_uint#}
type CLulong = {#type cl_ulong#}
type CLbool = {#type cl_bool#}

type CLPlatformInfo_ = {#type cl_platform_info#}
type CLDeviceType_ = {#type cl_device_type#}
type CLDeviceInfo_ = {#type cl_device_info#}
type CLDeviceFPConfig_ = {#type cl_device_fp_config#}
type CLDeviceMemCacheType_ = {#type cl_device_mem_cache_type#}
type CLDeviceLocalMemType_ = {#type cl_device_local_mem_type#}
type CLDeviceExecCapability_ = {#type cl_device_exec_capabilities#}
type CLContextInfo_ = {#type cl_context_info#}
type CLContextProperty_ = {#type cl_context_properties#}
type CLCommandQueueInfo_ = {#type cl_command_queue_info#}
type CLCommandQueueProperty_ ={#type cl_command_queue_properties#}

newtype ErrorCode = ErrorCode CInt deriving( Eq )

-- -----------------------------------------------------------------------------
data CLDeviceType = CL_DEVICE_TYPE_CPU 
                    -- ^ An OpenCL device that is the host processor. The host 
                    -- processor runs the OpenCL implementations and is a single 
                    -- or multi-core CPU.
                  | CL_DEVICE_TYPE_GPU	
                    -- ^ An OpenCL device that is a GPU. By this we mean that 
                    -- the device can also be used to accelerate a 3D API such 
                    -- as OpenGL or DirectX.
                  | CL_DEVICE_TYPE_ACCELERATOR	
                    -- ^ Dedicated OpenCL accelerators (for example the IBM CELL 
                    -- Blade). These devices communicate with the host processor 
                    -- using a peripheral interconnect such as PCIe.
                  | CL_DEVICE_TYPE_DEFAULT 
                    -- ^ The default OpenCL device in the system.                    
                  | CL_DEVICE_TYPE_ALL	
                    -- ^ All OpenCL devices available in the system.
                  deriving( Eq, Show )

deviceTypeValues :: [(CLDeviceType,CLDeviceType_)]
deviceTypeValues = [ 
  (CL_DEVICE_TYPE_CPU, 1 `shiftL` 1), (CL_DEVICE_TYPE_GPU, 1 `shiftL` 2), 
  (CL_DEVICE_TYPE_ACCELERATOR, 1 `shiftL` 3), (CL_DEVICE_TYPE_DEFAULT, 1 `shiftL` 0),
  (CL_DEVICE_TYPE_ALL, complement 0) ]
getDeviceTypeValue :: CLDeviceType -> CLDeviceType_
getDeviceTypeValue info = fromMaybe 0 (lookup info deviceTypeValues)

data CLCommandQueueProperty = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE	
                              -- ^ Determines whether the commands queued in the 
                              -- command-queue are executed in-order or 
                              -- out-of-order. If set, the commands in the 
                              -- command-queue are executed out-of-order. 
                              -- Otherwise, commands are executed in-order.
                            | CL_QUEUE_PROFILING_ENABLE	
                              -- ^ Enable or disable profiling of commands in
                              -- the command-queue. If set, the profiling of 
                              -- commands is enabled. Otherwise profiling of 
                              -- commands is disabled. See 
                              -- 'clGetEventProfilingInfo' for more information.
                              deriving( Eq, Show )

commandQueueProperties :: [(CLCommandQueueProperty,CLCommandQueueProperty_)]
commandQueueProperties = [
  (CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE, 1 `shiftL` 0),
  (CL_QUEUE_PROFILING_ENABLE, 1 `shiftL` 1)]

data CLDeviceFPConfig = CL_FP_DENORM -- ^ denorms are supported.
                      | CL_FP_INF_NAN -- ^ INF and NaNs are supported.
                      | CL_FP_ROUND_TO_NEAREST 
                        -- ^ round to nearest even rounding mode supported.
                      | CL_FP_ROUND_TO_ZERO 
                        -- ^ round to zero rounding mode supported.
                      | CL_FP_ROUND_TO_INF 
                        -- ^ round to +ve and -ve infinity rounding modes 
                        -- supported.
                      | CL_FP_FMA 
                        -- ^ IEEE754-2008 fused multiply-add is supported.
                        deriving( Show )
                        
deviceFPValues :: [(CLDeviceFPConfig,CLDeviceFPConfig_)]
deviceFPValues = [
  (CL_FP_DENORM, 1 `shiftL` 0), (CL_FP_INF_NAN, 1 `shiftL` 1),
  (CL_FP_ROUND_TO_NEAREST, 1 `shiftL` 2), (CL_FP_ROUND_TO_ZERO, 1 `shiftL` 3),
  (CL_FP_ROUND_TO_INF, 1 `shiftL` 4), (CL_FP_FMA, 1 `shiftL` 5)]

data CLDeviceExecCapability = CL_EXEC_KERNEL 
                              -- ^ The OpenCL device can execute OpenCL kernels.
                            | CL_EXEC_NATIVE_KERNEL
                              -- ^ The OpenCL device can execute native kernels.
                              deriving( Show )

deviceExecValues :: [(CLDeviceExecCapability,CLDeviceExecCapability_)]
deviceExecValues = [
  (CL_EXEC_KERNEL, 1 `shiftL` 0), (CL_EXEC_NATIVE_KERNEL, 1 `shiftL` 1)]
                   
data CLDeviceMemCacheType = CL_NONE | CL_READ_ONLY_CACHE | CL_READ_WRITE_CACHE
                          deriving( Show )
deviceMemCacheTypes :: [(CLDeviceMemCacheType_,CLDeviceMemCacheType)]
deviceMemCacheTypes = [
  (0x0,CL_NONE), (0x1,CL_READ_ONLY_CACHE),(0x2,CL_READ_WRITE_CACHE)]
getDeviceMemCacheType :: CLDeviceMemCacheType_ -> Maybe CLDeviceMemCacheType
getDeviceMemCacheType val = lookup val deviceMemCacheTypes

data CLDeviceLocalMemType = CL_LOCAL | CL_GLOBAL deriving( Show )

deviceLocalMemTypes :: [(CLDeviceLocalMemType_,CLDeviceLocalMemType)]
deviceLocalMemTypes = [(0x1,CL_LOCAL), (0x2,CL_GLOBAL)]
getDeviceLocalMemType :: CLDeviceLocalMemType_ -> Maybe CLDeviceLocalMemType
getDeviceLocalMemType val = lookup val deviceLocalMemTypes

-- -----------------------------------------------------------------------------
bitmaskToDeviceTypes :: CLDeviceType_ -> [CLDeviceType]
bitmaskToDeviceTypes mask = map fst . filter (testMask mask) $ deviceTypeValues

bitmaskFromDeviceTypes :: [CLDeviceType] -> CLDeviceType_
bitmaskFromDeviceTypes = foldl' (.|.) 0 . mapMaybe (`lookup` deviceTypeValues)
  
bitmaskToCommandQueueProperties :: CLCommandQueueProperty_ -> [CLCommandQueueProperty]
bitmaskToCommandQueueProperties mask = map fst . filter (testMask mask) $ commandQueueProperties
      
bitmaskFromCommandQueueProperties :: [CLCommandQueueProperty] -> CLCommandQueueProperty_
bitmaskFromCommandQueueProperties = foldl' (.|.) 0 . mapMaybe (`lookup` commandQueueProperties)

bitmaskToFPConfig :: CLDeviceFPConfig_ -> [CLDeviceFPConfig]
bitmaskToFPConfig mask = map fst . filter (testMask mask) $ deviceFPValues

bitmaskToExecCapability :: CLDeviceExecCapability_ -> [CLDeviceExecCapability]
bitmaskToExecCapability mask = map fst . filter (testMask mask) $ deviceExecValues

-- -----------------------------------------------------------------------------
