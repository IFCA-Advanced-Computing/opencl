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
  CLProfilingInfo(..), CLPlatformInfo(..), CLMemFlag(..),
  -- * Functions
  clSuccess, wrapPError, wrapCheckSuccess, getCLValue, getDeviceLocalMemType, 
  getDeviceMemCacheType, getCommandType, getCommandExecutionStatus, 
  bitmaskToDeviceTypes, bitmaskFromFlags, bitmaskToCommandQueueProperties, 
  bitmaskToFPConfig, bitmaskToExecCapability )
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

--type CLImageChannelOrder_ = {#type cl_channel_order#}
--type CLImageChannelDataType_ = {#type cl_channel_type#}

-- -----------------------------------------------------------------------------
newtype ErrorCode = ErrorCode CLint deriving( Eq )

clSuccess :: ErrorCode
clSuccess = ErrorCode (0)

-- -----------------------------------------------------------------------------
#c
enum CLError {
  cL_BUILD_PROGRAM_FAILURE=CL_BUILD_PROGRAM_FAILURE,
  cL_COMPILER_NOT_AVAILABLE=CL_COMPILER_NOT_AVAILABLE,
  cL_DEVICE_NOT_AVAILABLE=CL_DEVICE_NOT_AVAILABLE,
  cL_DEVICE_NOT_FOUND=CL_DEVICE_NOT_FOUND,
  cL_IMAGE_FORMAT_MISMATCH=CL_IMAGE_FORMAT_MISMATCH,
  cL_IMAGE_FORMAT_NOT_SUPPORTED=CL_IMAGE_FORMAT_NOT_SUPPORTED,
  cL_INVALID_ARG_INDEX=CL_INVALID_ARG_INDEX,
  cL_INVALID_ARG_SIZE=CL_INVALID_ARG_SIZE,
  cL_INVALID_ARG_VALUE=CL_INVALID_ARG_VALUE,
  cL_INVALID_BINARY=CL_INVALID_BINARY,
  cL_INVALID_BUFFER_SIZE=CL_INVALID_BUFFER_SIZE,
  cL_INVALID_BUILD_OPTIONS=CL_INVALID_BUILD_OPTIONS,
  cL_INVALID_COMMAND_QUEUE=CL_INVALID_COMMAND_QUEUE,
  cL_INVALID_CONTEXT=CL_INVALID_CONTEXT,
  cL_INVALID_DEVICE=CL_INVALID_DEVICE,
  cL_INVALID_DEVICE_TYPE=CL_INVALID_DEVICE_TYPE,
  cL_INVALID_EVENT=CL_INVALID_EVENT,
  cL_INVALID_EVENT_WAIT_LIST=CL_INVALID_EVENT_WAIT_LIST,
  cL_INVALID_GL_OBJECT=CL_INVALID_GL_OBJECT,
  cL_INVALID_GLOBAL_OFFSET=CL_INVALID_GLOBAL_OFFSET,
  cL_INVALID_HOST_PTR=CL_INVALID_HOST_PTR,
  cL_INVALID_IMAGE_FORMAT_DESCRIPTOR=CL_INVALID_IMAGE_FORMAT_DESCRIPTOR,
  cL_INVALID_IMAGE_SIZE=CL_INVALID_IMAGE_SIZE,
  cL_INVALID_KERNEL_NAME=CL_INVALID_KERNEL_NAME,
  cL_INVALID_KERNEL=CL_INVALID_KERNEL,
  cL_INVALID_KERNEL_ARGS=CL_INVALID_KERNEL_ARGS,
  cL_INVALID_KERNEL_DEFINITION=CL_INVALID_KERNEL_DEFINITION,
  cL_INVALID_MEM_OBJECT=CL_INVALID_MEM_OBJECT,
  cL_INVALID_OPERATION=CL_INVALID_OPERATION,
  cL_INVALID_PLATFORM=CL_INVALID_PLATFORM,
  cL_INVALID_PROGRAM=CL_INVALID_PROGRAM,
  cL_INVALID_PROGRAM_EXECUTABLE=CL_INVALID_PROGRAM_EXECUTABLE,
  cL_INVALID_QUEUE_PROPERTIES=CL_INVALID_QUEUE_PROPERTIES,
  cL_INVALID_SAMPLER=CL_INVALID_SAMPLER,
  cL_INVALID_VALUE=CL_INVALID_VALUE,
  cL_INVALID_WORK_DIMENSION=CL_INVALID_WORK_DIMENSION,
  cL_INVALID_WORK_GROUP_SIZE=CL_INVALID_WORK_GROUP_SIZE,
  cL_INVALID_WORK_ITEM_SIZE=CL_INVALID_WORK_ITEM_SIZE,
  cL_MAP_FAILURE=CL_MAP_FAILURE,
  cL_MEM_OBJECT_ALLOCATION_FAILURE=CL_MEM_OBJECT_ALLOCATION_FAILURE,
  cL_MEM_COPY_OVERLAP=CL_MEM_COPY_OVERLAP,
  cL_OUT_OF_HOST_MEMORY=CL_OUT_OF_HOST_MEMORY,
  cL_OUT_OF_RESOURCES=CL_OUT_OF_RESOURCES,
  cL_PROFILING_INFO_NOT_AVAILABLE=CL_PROFILING_INFO_NOT_AVAILABLE,
  cL_SUCCESS=CL_SUCCESS
  };
#endc

{-| 
 * 'CL_BUILD_PROGRAM_FAILURE', Returned if there is a failure to build the
program executable.

 * 'CL_COMPILER_NOT_AVAILABLE', Returned if the parameter program is created with
'clCreateProgramWithSource' and a compiler is not available. For example
'clDeviceCompilerAvalaible' is set to 'False'.

 * 'CL_DEVICE_NOT_AVAILABLE', Returned if the specified device is not currently
available.

 * 'CL_DEVICE_NOT_FOUND', Returned if no OpenCL devices that match the specified
devices were found.

 * 'CL_IMAGE_FORMAT_MISMATCH', Returned if the specified source and destination
images are not valid image objects.

 * 'CL_IMAGE_FORMAT_NOT_SUPPORTED', Returned if the specified image format is not
supported.

 * 'CL_INVALID_ARG_INDEX', Returned if an invalid argument index is specified.

 * 'CL_INVALID_ARG_SIZE', Returned if argument size specified (arg_size) does not
match the size of the data type for an argument that is not a memory object, or
if the argument is a memory object and arg_size != sizeof(cl_mem) or if arg_size
is zero and the argument is declared with the __local qualifier or if the
argument is a sampler and arg_size != sizeof(cl_sampler).

 * 'CL_INVALID_ARG_VALUE', Returned if the argument value specified is NULL for
an argument that is not declared with the __local qualifier or vice-versa.

 * 'CL_INVALID_BINARY', Returned if the program binary is not a valid binary for
the specified device.

 * 'CL_INVALID_BUFFER_SIZE', Returned if the value of the parameter size is 0 or
is greater than 'clDeviceMaxMemAllocSize' for all devices specified in the
parameter context.

 * 'CL_INVALID_BUILD_OPTIONS', Returned if the specified build options are
invalid.

 * 'CL_INVALID_COMMAND_QUEUE', Returned if the specified command-queue is not a
valid command-queue.

 * 'CL_INVALID_CONTEXT', Returned if the specified context is not a valid OpenCL
context, or the context associated with certain parameters are not the same.

 * 'CL_INVALID_DEVICE', Returned if the device or devices specified are not
valid.

 * 'CL_INVALID_DEVICE_TYPE', Returned if device type specified is not valid.

 * 'CL_INVALID_EVENT', Returned if the event objects specified are not valid.

 * 'CL_INVALID_EVENT_WAIT_LIST', Returned if event_wait_list is NULL and
num_events_in_wait_list > 0, or event_wait_list_list is not NULL and
num_events_in_wait_list is 0, or specified event objects are not valid events.

 * 'CL_INVALID_GL_OBJECT', Returned if obj is not a vaild GL object or is a GL
object but does not have an existing data store.

 * 'CL_INVALID_GLOBAL_OFFSET', Returned if global_work_offset is not NULL.

 * 'CL_INVALID_HOST_PTR', Returned if host_ptr is NULL and 'CL_MEM_USE_HOST_PTR'
or 'CL_MEM_COPY_HOST_PTR' are set in flags or if host_ptr is not NULL but
'CL_MEM_COPY_HOST_PTR' or 'CL_MEM_USE_HOST_PTR' are not set in flags.

 * 'CL_INVALID_IMAGE_FORMAT_DESCRIPTOR', Returned if the image format specified
is not valid or is NULL or does not map to a supported OpenCL image format.

 * 'CL_INVALID_IMAGE_SIZE', Returned if the specified image width or height are
invalid or if the image row pitch and image slice pitch do not follow the rules.

 * 'CL_INVALID_KERNEL_NAME', Returned if the specified kernel name is not found
in program.

 * 'CL_INVALID_KERNEL', Returned if the specified kernel is not a valid kernel
object.

 * 'CL_INVALID_KERNEL_ARGS', Returned if the kernel argument values have not been
specified.

 * 'CL_INVALID_KERNEL_DEFINITION', Returned if the function definition for
__kernel function given by kernel_name such as the number of arguments, the
argument types are not the same for all devices for which the program executable
has been built.

 * 'CL_INVALID_MEM_OBJECT', Returned if a parameter is not a valid memory, image,
or buffer object.

 * 'CL_INVALID_OPERATION', Returned if there are no devices in context that
support images. Returned if the build of a program executable for any of the
devices specified by a previous call to 'clBuildProgram' for program has not
completed, or if there are kernel objects attached to program. Returned by
'clEnqueueNativeKernel' if the specified device cannot execute the native
kernel.

 * 'CL_INVALID_PLATFORM', Returned if the specified platform is not a valid
platform, or no platform could be selected, or if platform value specified in
properties is not a valid platform.

 * 'CL_INVALID_PROGRAM', Returned if the specified program is not a valid program
object.

 * 'CL_INVALID_PROGRAM_EXECUTABLE', Returned if there is no successfully built
executable for program, or if there is no device in program. Returned if there
is no successfully built program executable available for device associated with
command_queue.

 * 'CL_INVALID_QUEUE_PROPERTIES', Returned if specified properties are valid but
are not supported by the device.

 * 'CL_INVALID_SAMPLER', Returned if the specified sampler is not a valid sampler
object, or for an argument declared to be of type sampler_t when the specified
arg_value is not a valid sampler object.

 * 'CL_INVALID_VALUE', Returned if a parameter is not an expected value.

 * 'CL_INVALID_WORK_DIMENSION', Returned if work_dim is not a valid value.

 * 'CL_INVALID_WORK_GROUP_SIZE', Returned if local_work_size is specified and
number of workitems specified by global_work_size is not evenly divisible by
size of work-group given by local_work_size or does not match the work-group
size specified for kernel using the __attribute__((reqd_work_group_size(X, Y,
Z))) qualifier in program source.

 * 'CL_INVALID_WORK_ITEM_SIZE', Returned if the number of work-items specified in
any of local_work_size... [0]... local_work_size[work_dim - 1] is greater than
the corresponding values specified by 'clDeviceMaxWorkItemSizes'.

 * 'CL_MAP_FAILURE', Returned by if there is a failure to map the requested
region into the host address space. This error cannot occur for buffer objects
created with 'CLMEM_USE_HOST_PTR' or 'CLMEM_ALLOC_HOST_PTR'.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE', Returned if there is a failure to allocate
memory for data store associated with image or buffer objects specified as
arguments to kernel.

 * 'CL_MEM_COPY_OVERLAP', Returned if the source and destination images are the
same image (or the source and destination buffers are the same buffer), and the
source and destination regions overlap.

 * 'CL_OUT_OF_HOST_MEMORY', Returned in the event of a failure to allocate
resources required by the OpenCL implementation on the host.

 * 'CL_OUT_OF_RESOURCES', Returned in the event of a failure to queue the
execution instance of kernel on the command-queue because of insufficient
resources needed to execute the kernel.

 * 'CL_PROFILING_INFO_NOT_AVAILABLE', Returned if the 'CL_QUEUE_PROFILING_ENABLE'
flag is not set for the command-queue and the profiling information is currently
not available (because the command identified by event has not completed).

 * 'CL_SUCCESS', Indicates that the function executed successfully.
-}
{#enum CLError {upcaseFirstLetter} deriving( Show, Eq ) #}

wrapPError :: (Ptr CLint -> IO a) -> IO (Either CLError a)
wrapPError f = alloca $ \perr -> do
  v <- f perr
  errcode <- peek perr >>= return . toEnum . fromIntegral
  if errcode == CL_SUCCESS
    then return $ Right v
    else return $ Left errcode
  
wrapCheckSuccess :: IO CLint -> IO Bool
wrapCheckSuccess f = f >>= return . (==CL_SUCCESS) . toEnum . fromIntegral

-- -----------------------------------------------------------------------------
#c
enum CLPlatformInfo {
  cL_PLATFORM_PROFILE=CL_PLATFORM_PROFILE,
  cL_PLATFORM_VERSION=CL_PLATFORM_VERSION,
  cL_PLATFORM_NAME=CL_PLATFORM_NAME,
  cL_PLATFORM_VENDOR=CL_PLATFORM_VENDOR,
  cL_PLATFORM_EXTENSIONS=CL_PLATFORM_EXTENSIONS
  };
#endc

{-|
 * 'CL_PLATFORM_PROFILE', OpenCL profile string. Returns the profile name 
supported by the implementation. The profile name returned can be one of the 
following strings:

 [@FULL_PROFILE@] If the implementation supports the OpenCL specification
(functionality defined as part of the core specification and does not require
any extensions to be supported).

 [@EMBEDDED_PROFILE@] If the implementation supports the OpenCL embedded 
profile. The embedded profile is  defined to be a subset for each version of 
OpenCL.

 * 'CL_PLATFORM_VERSION', OpenCL version string. Returns the OpenCL version 
supported by the implementation. This version string has the following format: 
/OpenCL major_version.minor_version platform-specific information/ The 
/major_version.minor_version/ value returned will be 1.0.
                    
 * 'CL_PLATFORM_NAME', Platform name string.
 
 * 'CL_PLATFORM_VENDOR', Platform vendor string.
                   
 * 'CL_PLATFORM_EXTENSIONS', Returns a space-separated list of extension names 
(the extension names themselves do not contain any spaces) supported by the 
platform. Extensions defined here must be supported by all devices associated 
with this platform.
-}
{#enum CLPlatformInfo {upcaseFirstLetter} deriving( Show ) #}

-- -----------------------------------------------------------------------------
#c
enum CLDeviceType {
  cL_DEVICE_TYPE_CPU=CL_DEVICE_TYPE_CPU,
  cL_DEVICE_TYPE_GPU=CL_DEVICE_TYPE_GPU,
  cL_DEVICE_TYPE_ACCELERATOR=CL_DEVICE_TYPE_ACCELERATOR,
  cL_DEVICE_TYPE_DEFAULT=CL_DEVICE_TYPE_DEFAULT,
  cL_DEVICE_TYPE_ALL=CL_DEVICE_TYPE_ALL
  };
#endc

{-|
 * 'CL_DEVICE_TYPE_CPU', An OpenCL device that is the host processor. The host 
processor runs the OpenCL implementations and is a single or multi-core CPU.
                  
 * 'CL_DEVICE_TYPE_GPU', An OpenCL device that is a GPU. By this we mean that the 
device can also be used to accelerate a 3D API such as OpenGL or DirectX.
                  
 * 'CL_DEVICE_TYPE_ACCELERATOR', Dedicated OpenCL accelerators (for example the 
IBM CELL Blade). These devices communicate with the host processor using a 
peripheral interconnect such as PCIe.
                
 * 'CL_DEVICE_TYPE_DEFAULT', The default OpenCL device in the system.
           
 * 'CL_DEVICE_TYPE_ALL', All OpenCL devices available in the system.
-}
{#enum CLDeviceType {upcaseFirstLetter} deriving( Show ) #}

#c
enum CLCommandQueueProperty { 
  cL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE=CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE,
  cL_QUEUE_PROFILING_ENABLE=CL_QUEUE_PROFILING_ENABLE
  };
#endc

{-|
 * 'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE', Determines whether the commands 
queued in the command-queue are executed in-order or out-of-order. If set, the 
commands in the command-queue are executed out-of-order. Otherwise, commands are 
executed in-order.
                            
 * 'CL_QUEUE_PROFILING_ENABLE', Enable or disable profiling of commands in the 
command-queue. If set, the profiling of commands is enabled. Otherwise profiling 
of commands is disabled. See 'clGetEventProfilingInfo' for more information.
-}
{#enum CLCommandQueueProperty {upcaseFirstLetter} deriving( Show, Bounded, Eq, Ord ) #}

#c
enum CLDeviceFPConfig {
  cL_FP_DENORM=CL_FP_DENORM, cL_FP_INF_NAN=CL_FP_INF_NAN,
  cL_FP_ROUND_TO_NEAREST=CL_FP_ROUND_TO_NEAREST,
  cL_FP_ROUND_TO_ZERO=CL_FP_ROUND_TO_ZERO,
  cL_FP_ROUND_TO_INF=CL_FP_ROUND_TO_INF, cL_FP_FMA=CL_FP_FMA,
  };
#endc

{-|
 * 'CL_FP_DENORM', denorms are supported.
                      
 * 'CL_FP_INF_NAN', INF and NaNs are supported.
                      
 * 'CL_FP_ROUND_TO_NEAREST', round to nearest even rounding mode supported.
                      
 * 'CL_FP_ROUND_TO_ZERO', round to zero rounding mode supported.
                      
 * 'CL_FP_ROUND_TO_INF', round to +ve and -ve infinity rounding modes supported.
                      
 * 'CL_FP_FMA', IEEE754-2008 fused multiply-add is supported.
-}
{#enum CLDeviceFPConfig {upcaseFirstLetter} deriving( Show, Bounded, Eq, Ord ) #}

#c
enum CLDeviceExecCapability {
  cL_EXEC_KERNEL=CL_EXEC_KERNEL,
  cL_EXEC_NATIVE_KERNEL=CL_EXEC_NATIVE_KERNEL
  };
#endc

{-|
 * 'CL_EXEC_KERNEL', The OpenCL device can execute OpenCL kernels.
                            
 * 'CL_EXEC_NATIVE_KERNEL', The OpenCL device can execute native kernels.
-}
{#enum CLDeviceExecCapability {upcaseFirstLetter} deriving( Show, Bounded, Eq, Ord ) #}

#c
enum CLDeviceMemCacheType {
  cL_NONE=CL_NONE, cL_READ_ONLY_CACHE=CL_READ_ONLY_CACHE,
  cL_READ_WRITE_CACHE=CL_READ_WRITE_CACHE
  };
#endc

{#enum CLDeviceMemCacheType {upcaseFirstLetter} deriving( Show ) #}

#c
enum CLDeviceLocalMemType {
  cL_LOCAL=CL_LOCAL, cL_GLOBAL=CL_GLOBAL
  };
#endc

{#enum CLDeviceLocalMemType {upcaseFirstLetter} deriving( Show ) #}

-- -----------------------------------------------------------------------------
#c
enum CLCommandType {
  cL_COMMAND_NDRANGE_KERNEL=CL_COMMAND_NDRANGE_KERNEL,
  cL_COMMAND_TASK=CL_COMMAND_TASK ,
  cL_COMMAND_NATIVE_KERNEL=CL_COMMAND_NATIVE_KERNEL,
  cL_COMMAND_READ_BUFFER=CL_COMMAND_READ_BUFFER,
  cL_COMMAND_WRITE_BUFFER=CL_COMMAND_WRITE_BUFFER,
  cL_COMMAND_COPY_BUFFER=CL_COMMAND_COPY_BUFFER,
  cL_COMMAND_READ_IMAGE=CL_COMMAND_READ_IMAGE,
  cL_COMMAND_WRITE_IMAGE=CL_COMMAND_WRITE_IMAGE,
  cL_COMMAND_COPY_IMAGE=CL_COMMAND_COPY_IMAGE,
  cL_COMMAND_COPY_BUFFER_TO_IMAGE=CL_COMMAND_COPY_BUFFER_TO_IMAGE,
  cL_COMMAND_COPY_IMAGE_TO_BUFFER=CL_COMMAND_COPY_IMAGE_TO_BUFFER,
  cL_COMMAND_MAP_BUFFER=CL_COMMAND_MAP_BUFFER,
  cL_COMMAND_MAP_IMAGE=CL_COMMAND_MAP_IMAGE,
  cL_COMMAND_UNMAP_MEM_OBJECT=CL_COMMAND_UNMAP_MEM_OBJECT,
  cL_COMMAND_MARKER=CL_COMMAND_MARKER,
  cL_COMMAND_ACQUIRE_GL_OBJECTS=CL_COMMAND_ACQUIRE_GL_OBJECTS,
  cL_COMMAND_RELEASE_GL_OBJECTS=CL_COMMAND_RELEASE_GL_OBJECTS
  };
#endc

-- | Command associated with an event.
{#enum CLCommandType {upcaseFirstLetter} deriving( Show ) #}

#c
enum CLCommandExecutionStatus {
  cL_QUEUED=CL_QUEUED, cL_SUBMITTED=CL_SUBMITTED, cL_RUNNING=CL_RUNNING,
  cL_COMPLETE=CL_COMPLETE, cL_EXEC_ERROR= -1
  };
#endc

{-|
 * 'CL_QUEUED', command has been enqueued in the command-queue.

 * 'CL_SUBMITTED', enqueued command has been submitted by the host to the 
device associated with the command-queue.

 * 'CL_RUNNING', device is currently executing this command.
                            
 * 'CL_COMPLETE', the command has completed.
                              
 * 'CL_EXEC_ERROR', command was abnormally terminated.
-}
{#enum CLCommandExecutionStatus {upcaseFirstLetter} deriving( Show ) #}

#c
enum CLProfilingInfo {
  cL_PROFILING_COMMAND_QUEUED=CL_PROFILING_COMMAND_QUEUED,
  cL_PROFILING_COMMAND_SUBMIT=CL_PROFILING_COMMAND_SUBMIT,
  cL_PROFILING_COMMAND_START=CL_PROFILING_COMMAND_START,
  cL_PROFILING_COMMAND_END=CL_PROFILING_COMMAND_END
  };
#endc

{-| Specifies the profiling data.

 * 'CL_PROFILING_COMMAND_QUEUED', A 64-bit value that describes the current 
device time counter in nanoseconds when the command identified by event is 
enqueued in a command-queue by the host.
 
 * 'CL_PROFILING_COMMAND_SUBMIT', A 64-bit value that describes the current 
device time counter in nanoseconds when the command identified by event that has 
been enqueued is submitted by the host to the device associated with the 
commandqueue.
 
 * 'CL_PROFILING_COMMAND_START', A 64-bit value that describes the current 
device time counter in nanoseconds when the command identified by event starts 
execution on the device.
 
 * 'CL_PROFILING_COMMAND_END', A 64-bit value that describes the current device 
time counter in nanoseconds when the command identified by event has finished 
execution on the device.
-}
{#enum CLProfilingInfo {upcaseFirstLetter} deriving( Show ) #}

-- -----------------------------------------------------------------------------
#c
enum CLMemFlag {
  cL_MEM_READ_WRITE=CL_MEM_READ_WRITE,
  cL_MEM_WRITE_ONLY=CL_MEM_WRITE_ONLY,
  cL_MEM_READ_ONLY=CL_MEM_READ_ONLY,
  cL_MEM_USE_HOST_PTR=CL_MEM_USE_HOST_PTR,
  cL_MEM_ALLOC_HOST_PTR=CL_MEM_ALLOC_HOST_PTR,
  cL_MEM_COPY_HOST_PTR=CL_MEM_COPY_HOST_PTR
  };
#endc
{-| 
 * 'CL_MEM_READ_WRITE', This flag specifies that the memory object will be
read and written by a kernel. This is the default.

 * 'CL_MEM_WRITE_ONLY', This flags specifies that the memory object will be
written but not read by a kernel. Reading from a buffer or image object created
with 'CLMEM_WRITE_ONLY' inside a kernel is undefined.

 * 'CL_MEM_READ_ONLY', This flag specifies that the memory object is a read-only
memory object when used inside a kernel. Writing to a buffer or image object
created with 'CLMEM_READ_ONLY' inside a kernel is undefined.

 * 'CL_MEM_USE_HOST_PTR', This flag is valid only if host_ptr is not NULL. If
specified, it indicates that the application wants the OpenCL implementation to
use memory referenced by host_ptr as the storage bits for the memory
object. OpenCL implementations are allowed to cache the buffer contents pointed
to by host_ptr in device memory. This cached copy can be used when kernels are
executed on a device. The result of OpenCL commands that operate on multiple
buffer objects created with the same host_ptr or overlapping host regions is
considered to be undefined.

 * 'CL_MEM_ALLOC_HOST_PTR', This flag specifies that the application wants the
OpenCL implementation to allocate memory from host accessible
memory. 'CL_MEM_ALLOC_HOST_PTR' and 'CL_MEM_USE_HOST_PTR' are mutually exclusive.

 * 'CL_MEM_COPY_HOST_PTR', This flag is valid only if host_ptr is not NULL. If
specified, it indicates that the application wants the OpenCL implementation to
allocate memory for the memory object and copy the data from memory referenced
by host_ptr. 'CL_MEM_COPY_HOST_PTR' and 'CL_MEM_USE_HOST_PTR' are mutually
exclusive. 'CL_MEM_COPY_HOST_PTR' can be used with 'CL_MEM_ALLOC_HOST_PTR' to
initialize the contents of the cl_mem object allocated using host-accessible
(e.g. PCIe) memory.  
-} 
{#enum CLMemFlag {upcaseFirstLetter} deriving( Show ) #}

-- -----------------------------------------------------------------------------
getCLValue :: (Enum a, Integral b) => a -> b
getCLValue = fromIntegral . fromEnum

getDeviceMemCacheType :: CLDeviceMemCacheType_ -> Maybe CLDeviceMemCacheType
getDeviceMemCacheType = Just . toEnum . fromIntegral

getDeviceLocalMemType :: CLDeviceLocalMemType_ -> Maybe CLDeviceLocalMemType
getDeviceLocalMemType = Just . toEnum . fromIntegral

getCommandType :: CLCommandType_ -> Maybe CLCommandType
getCommandType = Just . toEnum . fromIntegral

getCommandExecutionStatus :: CLint -> Maybe CLCommandExecutionStatus                                
getCommandExecutionStatus n 
  | n < 0 = Just CL_EXEC_ERROR
  | otherwise = Just . toEnum . fromIntegral $ n
                
-- -----------------------------------------------------------------------------
binaryFlags :: (Ord b, Enum b, Bounded b) => b -> [b]
binaryFlags m = map toEnum . takeWhile (<= (fromEnum m)) $ [1 `shiftL` n | n <- [0..]]
  
testMask :: Bits b => b -> b -> Bool
testMask mask v = (v .&. mask) == v

bitmaskFromFlags :: (Enum a, Bits b) => [a] -> b
bitmaskFromFlags = foldl' (.|.) 0 . map (fromIntegral . fromEnum)

bitmaskToDeviceTypes :: CLDeviceType_ -> [CLDeviceType]
bitmaskToDeviceTypes mask = filter (testMask mask . fromIntegral . fromEnum) $ [CL_DEVICE_TYPE_CPU,CL_DEVICE_TYPE_GPU,CL_DEVICE_TYPE_ACCELERATOR,CL_DEVICE_TYPE_DEFAULT,CL_DEVICE_TYPE_ALL]

bitmaskToCommandQueueProperties :: CLCommandQueueProperty_ -> [CLCommandQueueProperty]
bitmaskToCommandQueueProperties mask = filter (testMask mask . fromIntegral . fromEnum) $ binaryFlags maxBound
      
bitmaskToFPConfig :: CLDeviceFPConfig_ -> [CLDeviceFPConfig]
bitmaskToFPConfig mask = filter (testMask mask . fromIntegral . fromEnum) $ binaryFlags maxBound

bitmaskToExecCapability :: CLDeviceExecCapability_ -> [CLDeviceExecCapability]
bitmaskToExecCapability mask = filter (testMask mask . fromIntegral . fromEnum) $ binaryFlags maxBound

-- -----------------------------------------------------------------------------
