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
  CLEventInfo_, CLProfilingInfo_, CLCommandType(..), CLCommandType_, 
  CLCommandExecutionStatus(..), CLProfilingInfo(..), getProfilingInfoValue,
  CLCommandQueueProperty_, CLMemFlags_, CLImageFormat_p, CLMemObjectType_, 
  CLMemInfo_, CLImageInfo_, CLImageFormat(..), CLPlatformInfo(..), 
  getImageFormat, getDeviceTypeValue, getDeviceLocalMemType, 
  getDeviceMemCacheType, getCommandType, getCommandExecutionStatus, 
  bitmaskToDeviceTypes, bitmaskFromDeviceTypes, bitmaskToCommandQueueProperties, 
  bitmaskFromCommandQueueProperties, bitmaskToFPConfig, bitmaskToExecCapability, 
  getPlatformInfoValue )
       where

-- -----------------------------------------------------------------------------
import Foreign
import Foreign.C.Types
import Data.List( foldl' )

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
type CLCommandQueueProperty_ = {#type cl_command_queue_properties#}
type CLEventInfo_ = {#type cl_event_info#}
type CLProfilingInfo_ = {#type cl_profiling_info#}
type CLCommandType_ = {#type cl_command_type#}
type CLMemFlags_ = {#type cl_mem_flags#}
type CLMemObjectType_ = {#type cl_mem_object_type#}
type CLMemInfo_ = {#type cl_mem_info#}
type CLImageInfo_ = {#type cl_image_info#}
{#pointer *cl_image_format as CLImageFormat_p#}

type CLImageChannelOrder_ = {#type cl_channel_order#}
type CLImageChannelDataType_ = {#type cl_channel_type#}

newtype ErrorCode = ErrorCode CInt deriving( Eq )


-- -----------------------------------------------------------------------------
#c
enum CLPlatformInfo {
CLPLATFORM_PROFILE=CL_PLATFORM_PROFILE,
CLPLATFORM_VERSION=CL_PLATFORM_VERSION,
CLPLATFORM_NAME=CL_PLATFORM_NAME,
CLPLATFORM_VENDOR=CL_PLATFORM_VENDOR,
CLPLATFORM_EXTENSIONS=CL_PLATFORM_EXTENSIONS
  };
#endc

{-|
 * 'CLPLATFORM_PROFILE', OpenCL profile string. Returns the profile name 
supported by the implementation. The profile name returned can be one of the 
following strings:
 
 [@FULL_PROFILE@] If the implementation supports the OpenCL specification 
(functionality defined as part of the core specification and does not require 
any extensions to be supported).
 
 [@EMBEDDED_PROFILE@] If the implementation supports the OpenCL embedded 
profile. The embedded profile is  defined to be a subset for each version of 
OpenCL.
                    
 * 'CLPLATFORM_VERSION', OpenCL version string. Returns the OpenCL version 
supported by the implementation. This version string has the following format: 
/OpenCL major_version.minor_version platform-specific information/ The 
/major_version.minor_version/ value returned will be 1.0.
                    
 * 'CLPLATFORM_NAME', Platform name string.
 
 * 'CLPLATFORM_VENDOR', Platform vendor string.
                   
 * 'CLPLATFORM_EXTENSIONS', Returns a space-separated list of extension names 
(the extension names themselves do not contain any spaces) supported by the 
platform. Extensions defined here must be supported by all devices associated 
with this platform.
-}
{#enum CLPlatformInfo {} #}

getPlatformInfoValue :: CLPlatformInfo -> CLPlatformInfo_
getPlatformInfoValue = fromIntegral . fromEnum

-- -----------------------------------------------------------------------------
#c
enum CLDeviceType {
CLDEVICE_TYPE_CPU=CL_DEVICE_TYPE_CPU,
CLDEVICE_TYPE_GPU=CL_DEVICE_TYPE_GPU,
CLDEVICE_TYPE_ACCELERATOR=CL_DEVICE_TYPE_ACCELERATOR,
CLDEVICE_TYPE_DEFAULT=CL_DEVICE_TYPE_DEFAULT,
CLDEVICE_TYPE_ALL=CL_DEVICE_TYPE_ALL
  };
#endc

{-|
 * 'CLDEVICE_TYPE_CPU', An OpenCL device that is the host processor. The host 
processor runs the OpenCL implementations and is a single or multi-core CPU.
                  
 * 'CLDEVICE_TYPE_GPU', An OpenCL device that is a GPU. By this we mean that the 
device can also be used to accelerate a 3D API such as OpenGL or DirectX.
                  
 * 'CLDEVICE_TYPE_ACCELERATOR', Dedicated OpenCL accelerators (for example the 
IBM CELL Blade). These devices communicate with the host processor using a 
peripheral interconnect such as PCIe.
                
 * 'CLDEVICE_TYPE_DEFAULT', The default OpenCL device in the system.
           
 * 'CLDEVICE_TYPE_ALL', All OpenCL devices available in the system.
-}
{#enum CLDeviceType {} deriving( Show ) #}

getDeviceTypeValue :: CLDeviceType -> CLDeviceType_
getDeviceTypeValue = fromIntegral . fromEnum

#c
enum CLCommandQueueProperty { 
  CLQUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE=CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE,
  CLQUEUE_PROFILING_ENABLE=CL_QUEUE_PROFILING_ENABLE
  };
#endc

{-|
 * 'CLQUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE', Determines whether the commands 
queued in the command-queue are executed in-order or out-of-order. If set, the 
commands in the command-queue are executed out-of-order. Otherwise, commands are 
executed in-order.
                            
 * 'CLQUEUE_PROFILING_ENABLE', Enable or disable profiling of commands in the 
command-queue. If set, the profiling of commands is enabled. Otherwise profiling 
of commands is disabled. See 'clGetEventProfilingInfo' for more information.
-}
{#enum CLCommandQueueProperty {} deriving( Show, Bounded, Eq, Ord ) #}

#c
enum CLDeviceFPConfig {
  CLFP_DENORM=CL_FP_DENORM, CLFP_INF_NAN=CL_FP_INF_NAN,
  CLFP_ROUND_TO_NEAREST=CL_FP_ROUND_TO_NEAREST,
  CLFP_ROUND_TO_ZERO=CL_FP_ROUND_TO_ZERO,
  CLFP_ROUND_TO_INF=CL_FP_ROUND_TO_INF, CLFP_FMA=CL_FP_FMA,
  };
#endc

{-|
 * 'CLFP_DENORM', denorms are supported.
                      
 * 'CLFP_INF_NAN', INF and NaNs are supported.
                      
 * 'CLFP_ROUND_TO_NEAREST', round to nearest even rounding mode supported.
                      
 * 'CLFP_ROUND_TO_ZERO', round to zero rounding mode supported.
                      
 * 'CLFP_ROUND_TO_INF', round to +ve and -ve infinity rounding modes supported.
                      
 * 'CLFP_FMA', IEEE754-2008 fused multiply-add is supported.
-}
{#enum CLDeviceFPConfig {} deriving( Show, Bounded, Eq, Ord ) #}

#c
enum CLDeviceExecCapability {
  CLEXEC_KERNEL=CL_EXEC_KERNEL,
  CLEXEC_NATIVE_KERNEL=CL_EXEC_NATIVE_KERNEL
  };
#endc

{-|
 * 'CLEXEC_KERNEL', The OpenCL device can execute OpenCL kernels.
                            
 * 'CLEXEC_NATIVE_KERNEL', The OpenCL device can execute native kernels.
-}
{#enum CLDeviceExecCapability {} deriving( Show, Bounded, Eq, Ord ) #}

#c
enum CLDeviceMemCacheType {
  CLNONE=CL_NONE,CLREAD_ONLY_CACHE=CL_READ_ONLY_CACHE,
  CLREAD_WRITE_CACHE=CL_READ_WRITE_CACHE
  };
#endc

{#enum CLDeviceMemCacheType {} deriving( Show ) #}

getDeviceMemCacheType :: CLDeviceMemCacheType_ -> Maybe CLDeviceMemCacheType
getDeviceMemCacheType = Just . toEnum . fromIntegral

#c
enum CLDeviceLocalMemType {
  CLLOCAL=CL_LOCAL, CLGLOBAL=CL_GLOBAL
  };
#endc

{#enum CLDeviceLocalMemType {} deriving( Show ) #}

getDeviceLocalMemType :: CLDeviceLocalMemType_ -> Maybe CLDeviceLocalMemType
getDeviceLocalMemType = Just . toEnum . fromIntegral

-- -----------------------------------------------------------------------------
#c
enum CLCommandType {
  CLCOMMAND_NDRANGE_KERNEL=CL_COMMAND_NDRANGE_KERNEL,
  CLCOMMAND_TASK=CL_COMMAND_TASK ,
  CLCOMMAND_NATIVE_KERNEL=CL_COMMAND_NATIVE_KERNEL,
  CLCOMMAND_READ_BUFFER=CL_COMMAND_READ_BUFFER,
  CLCOMMAND_WRITE_BUFFER=CL_COMMAND_WRITE_BUFFER,
  CLCOMMAND_COPY_BUFFER=CL_COMMAND_COPY_BUFFER,
  CLCOMMAND_READ_IMAGE=CL_COMMAND_READ_IMAGE,
  CLCOMMAND_WRITE_IMAGE=CL_COMMAND_WRITE_IMAGE,
  CLCOMMAND_COPY_IMAGE=CL_COMMAND_COPY_IMAGE,
  CLCOMMAND_COPY_BUFFER_TO_IMAGE=CL_COMMAND_COPY_BUFFER_TO_IMAGE,
  CLCOMMAND_COPY_IMAGE_TO_BUFFER=CL_COMMAND_COPY_IMAGE_TO_BUFFER,
  CLCOMMAND_MAP_BUFFER=CL_COMMAND_MAP_BUFFER,
  CLCOMMAND_MAP_IMAGE=CL_COMMAND_MAP_IMAGE,
  CLCOMMAND_UNMAP_MEM_OBJECT=CL_COMMAND_UNMAP_MEM_OBJECT,
  CLCOMMAND_MARKER=CL_COMMAND_MARKER,
  CLCOMMAND_ACQUIRE_GL_OBJECTS=CL_COMMAND_ACQUIRE_GL_OBJECTS,
  CLCOMMAND_RELEASE_GL_OBJECTS=CL_COMMAND_RELEASE_GL_OBJECTS
  };
#endc

-- | Command associated with an event.
{#enum CLCommandType {} deriving( Show ) #}

getCommandType :: CLCommandType_ -> Maybe CLCommandType
getCommandType = Just . toEnum . fromIntegral

#c
enum CLCommandExecutionStatus {
  CLQUEUED=CL_QUEUED, CLSUBMITTED=CL_SUBMITTED, CLRUNNING=CL_RUNNING,
  CLCOMPLETE=CL_COMPLETE, CLEXEC_ERROR= -1
  };
#endc

{-|
 * 'CLQUEUED', command has been enqueued in the command-queue.

 * 'CLSUBMITTED', enqueued command has been submitted by the host to the 
device associated with the command-queue.

 * 'CLRUNNING', device is currently executing this command.
                            
 * 'CLCOMPLETE', the command has completed.
                              
 * 'CLEXEC_ERROR', command was abnormally terminated.
-}
{#enum CLCommandExecutionStatus {} deriving( Show ) #}

getCommandExecutionStatus :: CLint -> Maybe CLCommandExecutionStatus                                
getCommandExecutionStatus n 
  | n < 0 = Just CLEXEC_ERROR
  | otherwise = Just . toEnum . fromIntegral $ n
                
#c
enum CLProfilingInfo {
  CLPROFILING_COMMAND_QUEUED=CL_PROFILING_COMMAND_QUEUED,
  CLPROFILING_COMMAND_SUBMIT=CL_PROFILING_COMMAND_SUBMIT,
  CLPROFILING_COMMAND_START=CL_PROFILING_COMMAND_START,
  CLPROFILING_COMMAND_END=CL_PROFILING_COMMAND_END
  };
#endc

{-| Specifies the profiling data.

 * 'CLPROFILING_COMMAND_QUEUED', A 64-bit value that describes the current 
device time counter in nanoseconds when the command identified by event is 
enqueued in a command-queue by the host.
 
 * 'CLPROFILING_COMMAND_SUBMIT', A 64-bit value that describes the current 
device time counter in nanoseconds when the command identified by event that has 
been enqueued is submitted by the host to the device associated with the 
commandqueue.
 
 * 'CLPROFILING_COMMAND_START',	 A 64-bit value that describes the current 
device time counter in nanoseconds when the command identified by event starts 
execution on the device.
 
 * 'CLPROFILING_COMMAND_END', A 64-bit value that describes the current device 
time counter in nanoseconds when the command identified by event has finished 
execution on the device.
-}
{#enum CLProfilingInfo {} deriving( Show ) #}

getProfilingInfoValue :: CLProfilingInfo -> CLProfilingInfo_
getProfilingInfoValue = fromIntegral . fromEnum

-- -----------------------------------------------------------------------------
getImageChannelOrder :: CLImageFormat_p -> IO CLImageChannelOrder_
getImageChannelOrder = {#get cl_image_format->image_channel_order#}

getImageChannelDataType :: CLImageFormat_p -> IO CLImageChannelDataType_
getImageChannelDataType = {#get cl_image_format->image_channel_data_type#}

data CLImageFormat = CLImageFormat { 
  image_channel_order :: CLImageChannelOrder_,
  image_channel_data_type :: CLImageChannelDataType_
  } deriving( Show )

getImageFormat :: CLImageFormat_p -> IO CLImageFormat
getImageFormat p = do
  order <- getImageChannelOrder p
  datatype <- getImageChannelDataType p
  return $ CLImageFormat order datatype
  
-- -----------------------------------------------------------------------------
binaryFlags :: (Ord b, Enum b, Bounded b) => b -> [b]
binaryFlags m = map toEnum . takeWhile (<= (fromEnum m)) $ [1 `shiftL` n | n <- [0..]]
  
testMask :: Bits b => b -> b -> Bool
testMask mask v = (v .&. mask) == v

bitmaskToDeviceTypes :: CLDeviceType_ -> [CLDeviceType]
bitmaskToDeviceTypes mask = filter (testMask mask . fromIntegral . fromEnum) $ [CLDEVICE_TYPE_CPU,CLDEVICE_TYPE_GPU,CLDEVICE_TYPE_ACCELERATOR,CLDEVICE_TYPE_DEFAULT,CLDEVICE_TYPE_ALL]

bitmaskFromDeviceTypes :: [CLDeviceType] -> CLDeviceType_
bitmaskFromDeviceTypes = foldl' (.|.) 0 . map (fromIntegral . fromEnum)
  
bitmaskToCommandQueueProperties :: CLCommandQueueProperty_ -> [CLCommandQueueProperty]
bitmaskToCommandQueueProperties mask = filter (testMask mask . fromIntegral . fromEnum) $ binaryFlags maxBound
      
bitmaskFromCommandQueueProperties :: [CLCommandQueueProperty] -> CLCommandQueueProperty_
bitmaskFromCommandQueueProperties = foldl' (.|.) 0 . map (fromIntegral.fromEnum)

bitmaskToFPConfig :: CLDeviceFPConfig_ -> [CLDeviceFPConfig]
bitmaskToFPConfig mask = filter (testMask mask . fromIntegral . fromEnum) $ binaryFlags maxBound

bitmaskToExecCapability :: CLDeviceExecCapability_ -> [CLDeviceExecCapability]
bitmaskToExecCapability mask = filter (testMask mask . fromIntegral . fromEnum) $ binaryFlags maxBound

-- -----------------------------------------------------------------------------
