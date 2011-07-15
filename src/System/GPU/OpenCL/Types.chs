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
  -- * Symple CL Types
  CLbool, CLint, CLuint, CLulong, CLProgram, CLEvent, CLMem, CLPlatformID, 
  CLDeviceID, CLContext, CLCommandQueue, CLPlatformInfo_, CLDeviceType_, 
  CLDeviceInfo_, CLContextInfo_, CLContextProperty_, CLCommandQueueInfo_, 
  CLEventInfo_, CLProfilingInfo_, CLCommandType_, CLCommandQueueProperty_, 
  CLMemFlags_, CLImageFormat_p, CLMemObjectType_, CLMemInfo_, CLImageInfo_,
  -- * High Level Types
  CLError(..), ErrorCode(..), CLDeviceFPConfig(..), CLDeviceMemCacheType(..), 
  CLDeviceExecCapability(..), CLDeviceLocalMemType(..), CLDeviceType(..), 
  CLCommandQueueProperty(..), CLCommandType(..),  CLCommandExecutionStatus(..), 
  CLProfilingInfo(..), CLImageFormat(..), CLPlatformInfo(..),
  -- * Functions
  getProfilingInfoValue, getImageFormat, getDeviceTypeValue, 
  getDeviceLocalMemType, getDeviceMemCacheType, getCommandType, 
  getCommandExecutionStatus, bitmaskToDeviceTypes, bitmaskFromDeviceTypes, 
  bitmaskToCommandQueueProperties, bitmaskFromCommandQueueProperties, 
  bitmaskToFPConfig, bitmaskToExecCapability, getPlatformInfoValue )
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
enum CLError {
  CLBUILD_PROGRAM_FAILURE=CL_BUILD_PROGRAM_FAILURE,
  CLCOMPILER_NOT_AVAILABLE=CL_COMPILER_NOT_AVAILABLE,
  CLDEVICE_NOT_AVAILABLE=CL_DEVICE_NOT_AVAILABLE,
  CLDEVICE_NOT_FOUND=CL_DEVICE_NOT_FOUND,
  CLIMAGE_FORMAT_MISMATCH=CL_IMAGE_FORMAT_MISMATCH,
  CLIMAGE_FORMAT_NOT_SUPPORTED=CL_IMAGE_FORMAT_NOT_SUPPORTED,
  CLINVALID_ARG_INDEX=CL_INVALID_ARG_INDEX,
  CLINVALID_ARG_SIZE=CL_INVALID_ARG_SIZE,
  CLINVALID_ARG_VALUE=CL_INVALID_ARG_VALUE,
  CLINVALID_BINARY=CL_INVALID_BINARY,
  CLINVALID_BUFFER_SIZE=CL_INVALID_BUFFER_SIZE,
  CLINVALID_BUILD_OPTIONS=CL_INVALID_BUILD_OPTIONS,
  CLINVALID_COMMAND_QUEUE=CL_INVALID_COMMAND_QUEUE,
  CLINVALID_CONTEXT=CL_INVALID_CONTEXT,
  CLINVALID_DEVICE=CL_INVALID_DEVICE,
  CLINVALID_DEVICE_TYPE=CL_INVALID_DEVICE_TYPE,
  CLINVALID_EVENT=CL_INVALID_EVENT,
  CLINVALID_EVENT_WAIT_LIST=CL_INVALID_EVENT_WAIT_LIST,
  CLINVALID_GL_OBJECT=CL_INVALID_GL_OBJECT,
  CLINVALID_GLOBAL_OFFSET=CL_INVALID_GLOBAL_OFFSET,
  CLINVALID_HOST_PTR=CL_INVALID_HOST_PTR,
  CLINVALID_IMAGE_FORMAT_DESCRIPTOR=CL_INVALID_IMAGE_FORMAT_DESCRIPTOR,
  CLINVALID_IMAGE_SIZE=CL_INVALID_IMAGE_SIZE,
  CLINVALID_KERNEL_NAME=CL_INVALID_KERNEL_NAME,
  CLINVALID_KERNEL=CL_INVALID_KERNEL,
  CLINVALID_KERNEL_ARGS=CL_INVALID_KERNEL_ARGS,
  CLINVALID_KERNEL_DEFINITION=CL_INVALID_KERNEL_DEFINITION,
  CLINVALID_MEM_OBJECT=CL_INVALID_MEM_OBJECT,
  CLINVALID_OPERATION=CL_INVALID_OPERATION,
  CLINVALID_PLATFORM=CL_INVALID_PLATFORM,
  CLINVALID_PROGRAM=CL_INVALID_PROGRAM,
  CLINVALID_PROGRAM_EXECUTABLE=CL_INVALID_PROGRAM_EXECUTABLE,
  CLINVALID_QUEUE_PROPERTIES=CL_INVALID_QUEUE_PROPERTIES,
  CLINVALID_SAMPLER=CL_INVALID_SAMPLER,
  CLINVALID_VALUE=CL_INVALID_VALUE,
  CLINVALID_WORK_DIMENSION=CL_INVALID_WORK_DIMENSION,
  CLINVALID_WORK_GROUP_SIZE=CL_INVALID_WORK_GROUP_SIZE,
  CLINVALID_WORK_ITEM_SIZE=CL_INVALID_WORK_ITEM_SIZE,
  CLMAP_FAILURE=CL_MAP_FAILURE,
  CLMEM_OBJECT_ALLOCATION_FAILURE=CL_MEM_OBJECT_ALLOCATION_FAILURE,
  CLMEM_COPY_OVERLAP=CL_MEM_COPY_OVERLAP,
  CLOUT_OF_HOST_MEMORY=CL_OUT_OF_HOST_MEMORY,
  CLOUT_OF_RESOURCES=CL_OUT_OF_RESOURCES,
  CLPROFILING_INFO_NOT_AVAILABLE=CL_PROFILING_INFO_NOT_AVAILABLE,
  CLSUCCESS=CL_SUCCESS
  };
#endc


{-| 
* 'CLBUILD_PROGRAM_FAILURE', Returned if there is a failure to build the
program executable.

 * 'CLCOMPILER_NOT_AVAILABLE', Returned if the parameter program is created with
'clCreateProgramWithSource' and a compiler is not available. For example
'clDeviceCompilerAvalaible' is set to 'False'.

 * 'CLDEVICE_NOT_AVAILABLE', Returned if the specified device is not currently
available.

 * 'CLDEVICE_NOT_FOUND', Returned if no OpenCL devices that match the specified
devices were found.

 * 'CLIMAGE_FORMAT_MISMATCH', Returned if the specified source and destination
images are not valid image objects.

 * 'CLIMAGE_FORMAT_NOT_SUPPORTED', Returned if the specified image format is not
supported.

 * 'CLINVALID_ARG_INDEX', Returned if an invalid argument index is specified.

 * 'CLINVALID_ARG_SIZE', Returned if argument size specified (arg_size) does not
match the size of the data type for an argument that is not a memory object, or
if the argument is a memory object and arg_size != sizeof(cl_mem) or if arg_size
is zero and the argument is declared with the __local qualifier or if the
argument is a sampler and arg_size != sizeof(cl_sampler).

 * 'CLINVALID_ARG_VALUE', Returned if the argument value specified is NULL for
an argument that is not declared with the __local qualifier or vice-versa.

 * 'CLINVALID_BINARY', Returned if the program binary is not a valid binary for
the specified device.

 * 'CLINVALID_BUFFER_SIZE', Returned if the value of the parameter size is 0 or
is greater than 'clDeviceMaxMemAllocSize' for all devices specified in the
parameter context.

 * 'CLINVALID_BUILD_OPTIONS', Returned if the specified build options are
invalid.

 * 'CLINVALID_COMMAND_QUEUE', Returned if the specified command-queue is not a
valid command-queue.

 * 'CLINVALID_CONTEXT', Returned if the specified context is not a valid OpenCL
context, or the context associated with certain parameters are not the same.

 * 'CLINVALID_DEVICE', Returned if the device or devices specified are not
valid.

 * 'CLINVALID_DEVICE_TYPE', Returned if device type specified is not valid.

 * 'CLINVALID_EVENT', Returned if the event objects specified are not valid.

 * 'CLINVALID_EVENT_WAIT_LIST', Returned if event_wait_list is NULL and
num_events_in_wait_list > 0, or event_wait_list_list is not NULL and
num_events_in_wait_list is 0, or specified event objects are not valid events.

 * 'CLINVALID_GL_OBJECT', Returned if obj is not a vaild GL object or is a GL
object but does not have an existing data store.

 * 'CLINVALID_GLOBAL_OFFSET', Returned if global_work_offset is not NULL.

 * 'CLINVALID_HOST_PTR', Returned if host_ptr is NULL and 'CLMEM_USE_HOST_PTR'
or 'CLMEM_COPY_HOST_PTR' are set in flags or if host_ptr is not NULL but
'CLMEM_COPY_HOST_PTR' or 'CLMEM_USE_HOST_PTR' are not set in flags.

 * 'CLINVALID_IMAGE_FORMAT_DESCRIPTOR', Returned if the image format specified
is not valid or is NULL or does not map to a supported OpenCL image format.

 * 'CLINVALID_IMAGE_SIZE', Returned if the specified image width or height are
invalid or if the image row pitch and image slice pitch do not follow the rules.

 * 'CLINVALID_KERNEL_NAME', Returned if the specified kernel name is not found
in program.

 * 'CLINVALID_KERNEL', Returned if the specified kernel is not a valid kernel
object.

 * 'CLINVALID_KERNEL_ARGS', Returned if the kernel argument values have not been
specified.

 * 'CLINVALID_KERNEL_DEFINITION', Returned if the function definition for
__kernel function given by kernel_name such as the number of arguments, the
argument types are not the same for all devices for which the program executable
has been built.

 * 'CLINVALID_MEM_OBJECT', Returned if a parameter is not a valid memory, image,
or buffer object.

 * 'CLINVALID_OPERATION', Returned if there are no devices in context that
support images. Returned if the build of a program executable for any of the
devices specified by a previous call to 'clBuildProgram' for program has not
completed, or if there are kernel objects attached to program. Returned by
'clEnqueueNativeKernel' if the specified device cannot execute the native
kernel.

 * 'CLINVALID_PLATFORM', Returned if the specified platform is not a valid
platform, or no platform could be selected, or if platform value specified in
properties is not a valid platform.

 * 'CLINVALID_PROGRAM', Returned if the specified program is not a valid program
object.

 * 'CLINVALID_PROGRAM_EXECUTABLE', Returned if there is no successfully built
executable for program, or if there is no device in program. Returned if there
is no successfully built program executable available for device associated with
command_queue.

 * 'CLINVALID_QUEUE_PROPERTIES', Returned if specified properties are valid but
are not supported by the device.

 * 'CLINVALID_SAMPLER', Returned if the specified sampler is not a valid sampler
object, or for an argument declared to be of type sampler_t when the specified
arg_value is not a valid sampler object.

 * 'CLINVALID_VALUE', Returned if a parameter is not an expected value.

 * 'CLINVALID_WORK_DIMENSION', Returned if work_dim is not a valid value.

 * 'CLINVALID_WORK_GROUP_SIZE', Returned if local_work_size is specified and
number of workitems specified by global_work_size is not evenly divisible by
size of work-group given by local_work_size or does not match the work-group
size specified for kernel using the __attribute__((reqd_work_group_size(X, Y,
Z))) qualifier in program source.

 * 'CLINVALID_WORK_ITEM_SIZE', Returned if the number of work-items specified in
any of local_work_size... [0]... local_work_size[work_dim - 1] is greater than
the corresponding values specified by 'clDeviceMaxWorkItemSizes'.

 * 'CLMAP_FAILURE', Returned by if there is a failure to map the requested
region into the host address space. This error cannot occur for buffer objects
created with 'CLMEM_USE_HOST_PTR' or 'CLMEM_ALLOC_HOST_PTR'.

 * 'CLMEM_OBJECT_ALLOCATION_FAILURE', Returned if there is a failure to allocate
memory for data store associated with image or buffer objects specified as
arguments to kernel.

 * 'CLMEM_COPY_OVERLAP', Returned if the source and destination images are the
same image (or the source and destination buffers are the same buffer), and the
source and destination regions overlap.

 * 'CL_OUT_OF_HOST_MEMORY', Returned in the event of a failure to allocate
resources required by the OpenCL implementation on the host.

 * 'CLOUT_OF_RESOURCES', Returned in the event of a failure to queue the
execution instance of kernel on the command-queue because of insufficient
resources needed to execute the kernel.

 * 'CLPROFILING_INFO_NOT_AVAILABLE', Returned if the 'CLQUEUE_PROFILING_ENABLE'
flag is not set for the command-queue and the profiling information is currently
not available (because the command identified by event has not completed).

 * 'CLSUCCESS', Indicates that the function executed successfully.
-}
{#enum CLError {} deriving( Show ) #}

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
{#enum CLPlatformInfo {} deriving( Show ) #}

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
