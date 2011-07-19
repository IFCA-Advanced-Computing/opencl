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
module System.GPU.OpenCL.Query( 
  -- * Types
  CLPlatformInfo(..), CLPlatformID, CLDeviceID, CLDeviceType(..),
  CLDeviceFPConfig(..), CLDeviceExecCapability(..), CLDeviceLocalMemType(..),
  CLDeviceMemCacheType(..),
  -- * Platform Query Functions
  clGetPlatformIDs, clGetPlatformInfo, 
  -- * Device Query Functions
  clGetDeviceIDs, clGetDeviceExecutionCapabilities,
  clGetDeviceAddressBits, clGetDeviceAvailable, clGetDeviceCompilerAvailable, 
  clGetDeviceEndianLittle, clGetDeviceErrorCorrectionSupport, 
  clGetDeviceExtensions, clGetDeviceGlobalMemCacheSize, 
  clGetDeviceGlobalMemCachelineSize, clGetDeviceGlobalMemSize, 
  clGetDeviceImageSupport, clGetDeviceImage2DMaxHeight, 
  clGetDeviceImage2DMaxWidth, clGetDeviceImage3DMaxDepth, 
  clGetDeviceImage3DMaxHeight, clGetDeviceImage3DMaxWidth, 
  clGetDeviceLocalMemSize, clGetDeviceMaxClockFrequency, 
  clGetDeviceMaxComputeUnits, clGetDeviceMaxConstantArgs, 
  clGetDeviceMaxConstantBufferSize, clGetDeviceMaxMemAllocSize, 
  clGetDeviceMaxParameterSize, clGetDeviceMaxReadImageArgs, 
  clGetDeviceMaxSamplers, clGetDeviceMaxWorkGroupSize, 
  clGetDeviceMaxWorkItemDimensions, clGetDeviceMaxWorkItemSizes, 
  clGetDeviceMaxWriteImageArgs, clGetDeviceMemBaseAddrAlign, 
  clGetDeviceMinDataTypeAlignSize, clGetDeviceName, clGetDevicePlatform, 
  clGetDevicePreferredVectorWidthChar, clGetDevicePreferredVectorWidthShort, 
  clGetDevicePreferredVectorWidthInt, clGetDevicePreferredVectorWidthLong, 
  clGetDevicePreferredVectorWidthFloat, clGetDevicePreferredVectorWidthDouble, 
  clGetDeviceProfile, clGetDeviceProfilingTimerResolution, clGetDeviceVendor, 
  clGetDeviceVendorID, clGetDeviceVersion, clGetDeviceDriverVersion, 
  clGetDeviceSingleFPConfig, clGetDeviceDoubleFPConfig, 
  clGetDeviceHalfFPConfig, clGetDeviceLocalMemType, 
  clGetDeviceGlobalMemCacheType, clGetDeviceQueueProperties, clGetDeviceType )
       where

-- -----------------------------------------------------------------------------
import Data.Maybe( fromMaybe )
import Foreign( Ptr, nullPtr, castPtr, alloca, allocaArray, peek, peekArray )
import Foreign.C.String( CString, peekCString )
import Foreign.C.Types( CSize )
import Foreign.Storable( sizeOf )
import System.GPU.OpenCL.Types( 
  CLbool, CLint, CLuint, CLulong, CLPlatformInfo_, CLDeviceType_, 
  CLDeviceInfo_, CLDeviceFPConfig(..), CLDeviceExecCapability(..), ErrorCode(..),
  CLDeviceLocalMemType(..), CLDeviceMemCacheType(..), CLPlatformInfo(..),
  CLPlatformID, CLDeviceID, CLDeviceType(..), CLCommandQueueProperty, 
  getDeviceMemCacheType, getDeviceLocalMemType, getCLValue, clSuccess,
  bitmaskToDeviceTypes, bitmaskToCommandQueueProperties, 
  bitmaskToFPConfig, bitmaskToExecCapability )

-- -----------------------------------------------------------------------------
foreign import ccall "clGetPlatformIDs" raw_clGetPlatformIDs :: 
  CLuint -> Ptr CLPlatformID -> Ptr CLuint -> IO CLint
foreign import ccall "clGetPlatformInfo" raw_clGetPlatformInfo :: 
  CLPlatformID -> CLPlatformInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint 
foreign import ccall "clGetDeviceIDs" raw_clGetDeviceIDs :: 
  CLPlatformID -> CLDeviceType_ -> CLuint -> Ptr CLDeviceID -> Ptr CLuint -> IO CLint
foreign import ccall "clGetDeviceInfo" raw_clGetDeviceInfo :: 
  CLDeviceID -> CLDeviceInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

#include <CL/cl.h>

-- -----------------------------------------------------------------------------
getNumPlatforms :: IO (Maybe CLuint)
getNumPlatforms = alloca $ \(num_platforms :: Ptr CLuint) -> do
  errcode <- fmap ErrorCode $ raw_clGetPlatformIDs 0 nullPtr num_platforms
  if errcode == clSuccess
    then fmap Just $ peek num_platforms
    else return Nothing

-- | Obtain the list of platforms available. Returns the list if the function 
-- is executed successfully. Otherwise it returns the empty list.
clGetPlatformIDs :: IO [CLPlatformID]
clGetPlatformIDs = do
  nplats <- getNumPlatforms
  case nplats of
    Nothing -> return []
    Just n -> allocaArray (fromIntegral n) $ \(plats :: Ptr CLPlatformID) -> do
      errcode <- fmap ErrorCode $ raw_clGetPlatformIDs n plats nullPtr
      if errcode == clSuccess
        then peekArray (fromIntegral n) plats
        else return []

getPlatformInfoSize :: CLPlatformID -> CLuint -> IO (Maybe CSize)
getPlatformInfoSize platform infoid = alloca $ \(value_size :: Ptr CSize) -> do
  errcode <- fmap ErrorCode $ raw_clGetPlatformInfo platform infoid 0 nullPtr value_size
  if errcode == clSuccess
    then fmap Just $ peek value_size
    else return Nothing
  
-- | Get specific information about the OpenCL platform. It returns Nothing if
-- platform is not a valid platform.
clGetPlatformInfo :: CLPlatformID -> CLPlatformInfo -> IO (Maybe String)
clGetPlatformInfo platform infoid = do
  sval <- getPlatformInfoSize platform infoval
  case sval of
    Nothing -> return Nothing
    Just n -> allocaArray (fromIntegral n) $ \(buff :: CString) -> do
      errcode <- fmap ErrorCode $ raw_clGetPlatformInfo platform infoval n (castPtr buff) nullPtr
      if errcode == clSuccess
        then fmap Just $ peekCString buff
        else return Nothing
    where
      infoval = getCLValue infoid

-- -----------------------------------------------------------------------------
getNumDevices :: CLPlatformID -> CLDeviceType_ -> IO (Maybe CLuint)
getNumDevices platform dtype = alloca $ \(num_devices :: Ptr CLuint) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceIDs platform dtype 0 nullPtr num_devices
  if errcode == clSuccess
    then fmap Just $ peek num_devices
    else return Nothing

-- | Obtain the list of devices available on a platform. Returns the list if 
-- the function is executed successfully. Otherwise it returns the empty list 
-- if platform is not a valid platform or no OpenCL devices that matched 
-- device_type were found.
clGetDeviceIDs :: CLPlatformID -> CLDeviceType -> IO [CLDeviceID]
clGetDeviceIDs platform dtype = do
  ndevs <- getNumDevices platform dval
  case ndevs of
    Nothing -> return []
    Just n -> allocaArray (fromIntegral n) $ \(devs :: Ptr CLDeviceID) -> do
      errcode <- fmap ErrorCode $ raw_clGetDeviceIDs platform dval n devs nullPtr
      if errcode == clSuccess
        then peekArray (fromIntegral n) devs
        else return []
    where
      dval = getCLValue dtype

getDeviceInfoSize :: CLDeviceID -> CLDeviceInfo_ -> IO (Maybe CSize)
getDeviceInfoSize device infoid = alloca $ \(value_size :: Ptr CSize) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid 0 nullPtr value_size
  if errcode == clSuccess
    then fmap Just $ peek value_size
    else return Nothing
  
getDeviceInfoString :: CLDeviceInfo_ -> CLDeviceID -> IO (Maybe String)
getDeviceInfoString infoid device = do
  sval <- getDeviceInfoSize device infoid
  case sval of
    Nothing -> return Nothing
    Just n -> allocaArray (fromIntegral n) $ \(buff :: CString) -> do
      errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid n (castPtr buff) nullPtr
      if errcode == clSuccess
        then fmap Just $ peekCString buff
        else return Nothing
  
getDeviceInfoUint :: CLDeviceInfo_ -> CLDeviceID -> IO (Maybe CLuint)
getDeviceInfoUint infoid device = alloca $ \(dat :: Ptr CLuint) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CLuint)

getDeviceInfoUlong :: CLDeviceInfo_ -> CLDeviceID -> IO (Maybe CLulong)
getDeviceInfoUlong infoid device = alloca $ \(dat :: Ptr CLulong) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CLulong)

getDeviceInfoSizet :: CLDeviceInfo_ -> CLDeviceID -> IO (Maybe CSize)
getDeviceInfoSizet infoid device = alloca $ \(dat :: Ptr CSize) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CSize)
  
getDeviceInfoBool :: CLDeviceInfo_ -> CLDeviceID -> IO (Maybe Bool)
getDeviceInfoBool infoid device = alloca $ \(dat :: Ptr CLbool) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap (Just . (/=0)) $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CLbool)
  
getDeviceInfoFP :: CLDeviceInfo_ -> CLDeviceID -> IO [CLDeviceFPConfig]
getDeviceInfoFP infoid device = fmap (bitmaskToFPConfig . fromMaybe 0) $ getDeviceInfoUlong infoid device

#c
enum CLDeviceInfo {
  cL_DEVICE_ADDRESS_BITS=CL_DEVICE_ADDRESS_BITS,
  cL_DEVICE_AVAILABLE=CL_DEVICE_AVAILABLE,
  cL_DEVICE_COMPILER_AVAILABLE=CL_DEVICE_COMPILER_AVAILABLE,
#ifdef CL_DEVICE_DOUBLE_FP_CONFIG  
  cL_DEVICE_DOUBLE_FP_CONFIG=CL_DEVICE_DOUBLE_FP_CONFIG,
#else
  cL_DEVICE_DOUBLE_FP_CONFIG=0x1032,
#endif
  cL_DEVICE_ENDIAN_LITTLE=CL_DEVICE_ENDIAN_LITTLE,
  cL_DEVICE_ERROR_CORRECTION_SUPPORT=CL_DEVICE_ERROR_CORRECTION_SUPPORT,
  cL_DEVICE_EXECUTION_CAPABILITIES=CL_DEVICE_EXECUTION_CAPABILITIES,
  cL_DEVICE_EXTENSIONS=CL_DEVICE_EXTENSIONS,
  cL_DEVICE_GLOBAL_MEM_CACHE_SIZE=CL_DEVICE_GLOBAL_MEM_CACHE_SIZE,
  cL_DEVICE_GLOBAL_MEM_CACHE_TYPE=CL_DEVICE_GLOBAL_MEM_CACHE_TYPE,
  cL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE=CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE,
  cL_DEVICE_GLOBAL_MEM_SIZE=CL_DEVICE_GLOBAL_MEM_SIZE,
#ifdef CL_DEVICE_HALF_FP_CONFIG
  cL_DEVICE_HALF_FP_CONFIG=CL_DEVICE_HALF_FP_CONFIG,
#else
  cL_DEVICE_HALF_FP_CONFIG=0x1033,
#endif
  cL_DEVICE_IMAGE_SUPPORT=CL_DEVICE_IMAGE_SUPPORT,
  cL_DEVICE_IMAGE2D_MAX_HEIGHT=CL_DEVICE_IMAGE2D_MAX_HEIGHT,
  cL_DEVICE_IMAGE2D_MAX_WIDTH=CL_DEVICE_IMAGE2D_MAX_WIDTH,
  cL_DEVICE_IMAGE3D_MAX_DEPTH=CL_DEVICE_IMAGE3D_MAX_DEPTH,
  cL_DEVICE_IMAGE3D_MAX_HEIGHT=CL_DEVICE_IMAGE3D_MAX_HEIGHT,
  cL_DEVICE_IMAGE3D_MAX_WIDTH=CL_DEVICE_IMAGE3D_MAX_WIDTH,
  cL_DEVICE_LOCAL_MEM_SIZE=CL_DEVICE_LOCAL_MEM_SIZE,
  cL_DEVICE_LOCAL_MEM_TYPE=CL_DEVICE_LOCAL_MEM_TYPE,
  cL_DEVICE_MAX_CLOCK_FREQUENCY=CL_DEVICE_MAX_CLOCK_FREQUENCY,
  cL_DEVICE_MAX_COMPUTE_UNITS=CL_DEVICE_MAX_COMPUTE_UNITS,
  cL_DEVICE_MAX_CONSTANT_ARGS=CL_DEVICE_MAX_CONSTANT_ARGS,
  cL_DEVICE_MAX_CONSTANT_BUFFER_SIZE=CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE,
  cL_DEVICE_MAX_MEM_ALLOC_SIZE=CL_DEVICE_MAX_MEM_ALLOC_SIZE,
  cL_DEVICE_MAX_PARAMETER_SIZE=CL_DEVICE_MAX_PARAMETER_SIZE,
  cL_DEVICE_MAX_READ_IMAGE_ARGS=CL_DEVICE_MAX_READ_IMAGE_ARGS,
  cL_DEVICE_MAX_SAMPLERS=CL_DEVICE_MAX_SAMPLERS,
  cL_DEVICE_MAX_WORK_GROUP_SIZE=CL_DEVICE_MAX_WORK_GROUP_SIZE,
  cL_DEVICE_MAX_WORK_ITEM_DIMENSIONS=CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS,
  cL_DEVICE_MAX_WORK_ITEM_SIZES=CL_DEVICE_MAX_WORK_ITEM_SIZES,
  cL_DEVICE_MAX_WRITE_IMAGE_ARGS=CL_DEVICE_MAX_WRITE_IMAGE_ARGS,
  cL_DEVICE_MEM_BASE_ADDR_ALIGN=CL_DEVICE_MEM_BASE_ADDR_ALIGN,
  cL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE=CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE,
  cL_DEVICE_NAME=CL_DEVICE_NAME,
  cL_DEVICE_PLATFORM=CL_DEVICE_PLATFORM,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR=CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT=CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_INT=CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG=CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT=CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT,
  cL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE=CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
  cL_DEVICE_PROFILE=CL_DEVICE_PROFILE,
  cL_DEVICE_PROFILING_TIMER_RESOLUTION=CL_DEVICE_PROFILING_TIMER_RESOLUTION,
  cL_DEVICE_QUEUE_PROPERTIES=CL_DEVICE_QUEUE_PROPERTIES,
  cL_DEVICE_SINGLE_FP_CONFIG=CL_DEVICE_SINGLE_FP_CONFIG,
  cL_DEVICE_TYPE=CL_DEVICE_TYPE,
  cL_DEVICE_VENDOR=CL_DEVICE_VENDOR,
  cL_DEVICE_VENDOR_ID=CL_DEVICE_VENDOR_ID,
  cL_DEVICE_VERSION=CL_DEVICE_VERSION,
  cL_DRIVER_VERSION=CL_DRIVER_VERSION,
  };
#endc
{#enum CLDeviceInfo {upcaseFirstLetter} #}

-- | The default compute device address space size specified as an unsigned 
-- integer value in bits. Currently supported values are 32 or 64 bits.
clGetDeviceAddressBits :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceAddressBits = getDeviceInfoUint . getCLValue $ CL_DEVICE_ADDRESS_BITS

-- | Is 'True' if the device is available and 'False' if the device is not 
-- available.
clGetDeviceAvailable :: CLDeviceID -> IO (Maybe Bool)
clGetDeviceAvailable = getDeviceInfoBool . getCLValue $ CL_DEVICE_AVAILABLE

-- | Is 'False' if the implementation does not have a compiler available to 
-- compile the program source. Is 'True' if the compiler is available. This can 
-- be 'False' for the embededed platform profile only.
clGetDeviceCompilerAvailable :: CLDeviceID -> IO (Maybe Bool)
clGetDeviceCompilerAvailable = getDeviceInfoBool . getCLValue $ CL_DEVICE_COMPILER_AVAILABLE

-- | Describes the OPTIONAL double precision floating-point capability of the 
-- OpenCL device. This is a bit-field that describes one or more of the 
-- 'CLDeviceFPConfig' values.
-- The mandated minimum double precision floating-point capability is 
-- 'CL_FP_FMA' | 'CL_FP_ROUND_TO_NEAREST' | 'CL_FP_ROUND_TO_ZERO' | 
-- 'CL_FP_ROUND_TO_INF' | 'CL_FP_INF_NAN' | 'CL_FP_DENORM'.
clGetDeviceDoubleFPConfig :: CLDeviceID -> IO [CLDeviceFPConfig]
clGetDeviceDoubleFPConfig = getDeviceInfoFP . getCLValue $ CL_DEVICE_DOUBLE_FP_CONFIG

-- | Is 'True' if the OpenCL device is a little endian device and 'False' 
-- otherwise.
clGetDeviceEndianLittle :: CLDeviceID -> IO (Maybe Bool)
clGetDeviceEndianLittle = getDeviceInfoBool . getCLValue $ CL_DEVICE_ENDIAN_LITTLE

-- | Is 'True' if the device implements error correction for the memories, 
-- caches, registers etc. in the device. Is 'False' if the device does not 
-- implement error correction. This can be a requirement for certain clients of 
-- OpenCL.
clGetDeviceErrorCorrectionSupport :: CLDeviceID -> IO (Maybe Bool)
clGetDeviceErrorCorrectionSupport = getDeviceInfoBool . getCLValue $ CL_DEVICE_ERROR_CORRECTION_SUPPORT

-- | Describes the execution capabilities of the device. This is a list that 
-- describes one or more of the 'CLDeviceExecCapability' values.
-- The mandated minimum capability is 'CL_EXEC_KERNEL'.
clGetDeviceExecutionCapabilities :: CLDeviceID -> IO [CLDeviceExecCapability]
clGetDeviceExecutionCapabilities device = fmap (bitmaskToExecCapability . fromMaybe 0) $ getDeviceInfoUlong (getCLValue CL_DEVICE_EXECUTION_CAPABILITIES) device

-- | Returns a space separated list of extension names (the extension names 
-- themselves do not contain any spaces). The list of extension names returned 
-- currently can include one or more of the following approved extension names:
--
-- * cl_khr_fp64
--
-- * cl_khr_select_fprounding_mode
--
-- * cl_khr_global_int32_base_atomics
--
-- * cl_khr_global_int32_extended_atomics
--
-- * cl_khr_local_int32_base_atomics
--
-- * cl_khr_local_int32_extended_atomics
--
-- * cl_khr_int64_base_atomics
--
-- * cl_khr_int64_extended_atomics
--
-- * cl_khr_3d_image_writes
--
-- * cl_khr_byte_addressable_store
--
-- * cl_khr_fp16
clGetDeviceExtensions :: CLDeviceID -> IO (Maybe String)
clGetDeviceExtensions = getDeviceInfoString . getCLValue $ CL_DEVICE_EXTENSIONS

-- | Size of global memory cache in bytes.
clGetDeviceGlobalMemCacheSize :: CLDeviceID -> IO (Maybe CLulong)
clGetDeviceGlobalMemCacheSize = getDeviceInfoUlong . getCLValue $ CL_DEVICE_GLOBAL_MEM_CACHE_SIZE

-- | Type of global memory cache supported. Valid values are: 'CL_NONE', 
-- 'CL_READ_ONLY_CACHE', and 'CL_READ_WRITE_CACHE'.
clGetDeviceGlobalMemCacheType :: CLDeviceID -> IO (Maybe CLDeviceMemCacheType)
clGetDeviceGlobalMemCacheType device = getDeviceInfoUint (getCLValue CL_DEVICE_GLOBAL_MEM_CACHE_TYPE) device 
                                       >>= return . maybe Nothing getDeviceMemCacheType

-- | Size of global memory cache line in bytes.
clGetDeviceGlobalMemCachelineSize :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceGlobalMemCachelineSize = getDeviceInfoUint . getCLValue $ CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE

-- | Size of global device memory in bytes.
clGetDeviceGlobalMemSize :: CLDeviceID -> IO (Maybe CLulong)
clGetDeviceGlobalMemSize = getDeviceInfoUlong . getCLValue $ CL_DEVICE_GLOBAL_MEM_SIZE

-- | Describes the OPTIONAL half precision floating-point capability of the 
-- OpenCL device. This is a bit-field that describes one or more of the 
-- 'CLDeviceFPConfig' values.
-- The required minimum half precision floating-point capability as implemented 
-- by this extension is 'CL_FP_ROUND_TO_ZERO' | 'CL_FP_ROUND_TO_INF' | 
-- 'CL_FP_INF_NAN'.
clGetDeviceHalfFPConfig :: CLDeviceID -> IO [CLDeviceFPConfig]
clGetDeviceHalfFPConfig = getDeviceInfoFP . getCLValue $ CL_DEVICE_HALF_FP_CONFIG

-- | Is 'True' if images are supported by the OpenCL device and 'False' otherwise.
clGetDeviceImageSupport :: CLDeviceID -> IO (Maybe Bool)
clGetDeviceImageSupport = getDeviceInfoBool . getCLValue $ CL_DEVICE_IMAGE_SUPPORT

-- | Max height of 2D image in pixels. The minimum value is 8192 if
-- 'clGetDeviceImageSupport' is 'True'.
clGetDeviceImage2DMaxHeight :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceImage2DMaxHeight = getDeviceInfoSizet . getCLValue $ CL_DEVICE_IMAGE2D_MAX_HEIGHT

-- | Max width of 2D image in pixels. The minimum value is 8192 if
-- 'clGetDeviceImageSupport' is 'True'.
clGetDeviceImage2DMaxWidth :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceImage2DMaxWidth = getDeviceInfoSizet . getCLValue $ CL_DEVICE_IMAGE2D_MAX_WIDTH

-- | Max depth of 3D image in pixels. The minimum value is 2048 if 
-- 'clGetDeviceImageSupport' is 'True'.
clGetDeviceImage3DMaxDepth :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceImage3DMaxDepth = getDeviceInfoSizet . getCLValue $ CL_DEVICE_IMAGE3D_MAX_DEPTH

-- | Max height of 3D image in pixels. The minimum value is 2048 if 
-- 'clGetDeviceImageSupport' is 'True'.
clGetDeviceImage3DMaxHeight :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceImage3DMaxHeight = getDeviceInfoSizet . getCLValue $ CL_DEVICE_IMAGE3D_MAX_HEIGHT

-- | Max width of 3D image in pixels. The minimum value is 2048 if 
-- 'clGetDeviceImageSupport' is 'True'.
clGetDeviceImage3DMaxWidth :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceImage3DMaxWidth = getDeviceInfoSizet . getCLValue $ CL_DEVICE_IMAGE3D_MAX_WIDTH

-- | Size of local memory arena in bytes. The minimum value is 16 KB.
clGetDeviceLocalMemSize :: CLDeviceID -> IO (Maybe CLulong)
clGetDeviceLocalMemSize = getDeviceInfoUlong . getCLValue $ CL_DEVICE_LOCAL_MEM_SIZE

-- | Type of local memory supported. This can be set to 'CL_LOCAL' implying 
-- dedicated local memory storage such as SRAM, or 'CL_GLOBAL'.
clGetDeviceLocalMemType :: CLDeviceID -> IO (Maybe CLDeviceLocalMemType)
clGetDeviceLocalMemType device = getDeviceInfoUint (getCLValue CL_DEVICE_LOCAL_MEM_TYPE) device >>= return . maybe Nothing getDeviceLocalMemType

-- | Maximum configured clock frequency of the device in MHz.
clGetDeviceMaxClockFrequency :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceMaxClockFrequency = getDeviceInfoUint . getCLValue $ CL_DEVICE_MAX_CLOCK_FREQUENCY

-- | The number of parallel compute cores on the OpenCL device. The minimum 
-- value is 1.
clGetDeviceMaxComputeUnits :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceMaxComputeUnits = getDeviceInfoUint . getCLValue $ CL_DEVICE_MAX_COMPUTE_UNITS

-- | Max number of arguments declared with the __constant qualifier in a kernel. 
-- The minimum value is 8.
clGetDeviceMaxConstantArgs :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceMaxConstantArgs = getDeviceInfoUint . getCLValue $ CL_DEVICE_MAX_CONSTANT_ARGS

-- | Max size in bytes of a constant buffer allocation. The minimum value is 
-- 64 KB.
clGetDeviceMaxConstantBufferSize :: CLDeviceID -> IO (Maybe CLulong)
clGetDeviceMaxConstantBufferSize = getDeviceInfoUlong . getCLValue $ CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE

-- | Max size of memory object allocation in bytes. The minimum value is max 
-- (1/4th of 'clGetDeviceGlobalMemSize', 128*1024*1024)
clGetDeviceMaxMemAllocSize :: CLDeviceID -> IO (Maybe CLulong)
clGetDeviceMaxMemAllocSize = getDeviceInfoUlong . getCLValue $ CL_DEVICE_MAX_MEM_ALLOC_SIZE

-- | Max size in bytes of the arguments that can be passed to a kernel. The 
-- minimum value is 256.
clGetDeviceMaxParameterSize :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceMaxParameterSize = getDeviceInfoSizet . getCLValue $ CL_DEVICE_MAX_PARAMETER_SIZE

-- | Max number of simultaneous image objects that can be read by a kernel. The 
-- minimum value is 128 if 'clGetDeviceImageSupport' is 'True'.
clGetDeviceMaxReadImageArgs :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceMaxReadImageArgs = getDeviceInfoUint . getCLValue $ CL_DEVICE_MAX_READ_IMAGE_ARGS

-- | Maximum number of samplers that can be used in a kernel. The minimum value 
-- is 16 if 'clGetDeviceImageSupport' is 'True'. (Also see sampler type.)
clGetDeviceMaxSamplers :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceMaxSamplers = getDeviceInfoUint . getCLValue $ CL_DEVICE_MAX_SAMPLERS

-- | Maximum number of work-items in a work-group executing a kernel using the 
-- data parallel execution model. (Refer to 'clEnqueueNDRangeKernel'). The 
-- minimum value is 1.
clGetDeviceMaxWorkGroupSize :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceMaxWorkGroupSize = getDeviceInfoSizet . getCLValue $ CL_DEVICE_MAX_WORK_GROUP_SIZE

-- | Maximum dimensions that specify the global and local work-item IDs used by 
-- the data parallel execution model. (Refer to 'clEnqueueNDRangeKernel'). 
-- The minimum value is 3.
clGetDeviceMaxWorkItemDimensions :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceMaxWorkItemDimensions = getDeviceInfoUint . getCLValue $ CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS

-- | Maximum number of work-items that can be specified in each dimension of the 
-- work-group to 'clEnqueueNDRangeKernel'.
-- Returns n entries, where n is the value returned by the query for 
-- clDeviceMaxWorkItemDimensions. The minimum value is (1, 1, 1).
clGetDeviceMaxWorkItemSizes :: CLDeviceID -> IO [CSize]
clGetDeviceMaxWorkItemSizes device = do
  val <- clGetDeviceMaxWorkItemDimensions device
  case val of
    Nothing -> return []
    Just n -> allocaArray (fromIntegral n) $ \(buff :: Ptr CSize) -> do
      errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid (size n) (castPtr buff) nullPtr
      if errcode == clSuccess
        then peekArray (fromIntegral n) buff
        else return []
    where
      infoid = getCLValue CL_DEVICE_MAX_WORK_ITEM_SIZES
      size n = fromIntegral $ (fromIntegral n) * (sizeOf (0::CSize))

-- | Max number of simultaneous image objects that can be written to by a 
-- kernel. The minimum value is 8 if 'clGetDeviceImageSupport' is 'True'.
clGetDeviceMaxWriteImageArgs :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceMaxWriteImageArgs = getDeviceInfoUint . getCLValue $ CL_DEVICE_MAX_WRITE_IMAGE_ARGS

-- | Describes the alignment in bits of the base address of any allocated 
-- memory object.
clGetDeviceMemBaseAddrAlign :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceMemBaseAddrAlign = getDeviceInfoUint . getCLValue $ CL_DEVICE_MEM_BASE_ADDR_ALIGN

-- | The smallest alignment in bytes which can be used for any data type.
clGetDeviceMinDataTypeAlignSize :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceMinDataTypeAlignSize = getDeviceInfoUint . getCLValue $ CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE

-- | Device name string.
clGetDeviceName :: CLDeviceID -> IO (Maybe String)
clGetDeviceName = getDeviceInfoString . getCLValue $ CL_DEVICE_NAME

-- | The platform associated with this device.
clGetDevicePlatform :: CLDeviceID -> IO (Maybe CLPlatformID)
clGetDevicePlatform device = alloca $ \(dat :: Ptr CLPlatformID) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      infoid = getCLValue CL_DEVICE_PLATFORM
      size = fromIntegral $ sizeOf (nullPtr :: CLPlatformID)

-- | Preferred native vector width size for built-in char types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
clGetDevicePreferredVectorWidthChar :: CLDeviceID -> IO (Maybe CLuint)
clGetDevicePreferredVectorWidthChar = getDeviceInfoUint . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR

-- | Preferred native vector width size for built-in short types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
clGetDevicePreferredVectorWidthShort :: CLDeviceID -> IO (Maybe CLuint)
clGetDevicePreferredVectorWidthShort = getDeviceInfoUint . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT

-- | Preferred native vector width size for built-in int types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
clGetDevicePreferredVectorWidthInt :: CLDeviceID -> IO (Maybe CLuint)
clGetDevicePreferredVectorWidthInt = getDeviceInfoUint . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT

-- | Preferred native vector width size for built-in long types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
clGetDevicePreferredVectorWidthLong :: CLDeviceID -> IO (Maybe CLuint)
clGetDevicePreferredVectorWidthLong = getDeviceInfoUint . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG

-- | Preferred native vector width size for built-in float types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
clGetDevicePreferredVectorWidthFloat :: CLDeviceID -> IO (Maybe CLuint)
clGetDevicePreferredVectorWidthFloat = getDeviceInfoUint . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT

-- | Preferred native vector width size for built-in double types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
-- | If the cl_khr_fp64 extension is not supported, 
-- 'clGetDevicePreferredVectorWidthDouble' must return 0.
clGetDevicePreferredVectorWidthDouble :: CLDeviceID -> IO (Maybe CLuint)
clGetDevicePreferredVectorWidthDouble = getDeviceInfoUint . getCLValue $ CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE

-- | OpenCL profile string. Returns the profile name supported by the device 
-- (see note). The profile name returned can be one of the following strings:
-- 
-- * FULL_PROFILE - if the device supports the OpenCL specification 
-- (functionality defined as part of the core specification and does not require 
-- any extensions to be supported).
-- 
-- * EMBEDDED_PROFILE - if the device supports the OpenCL embedded profile.
clGetDeviceProfile :: CLDeviceID -> IO (Maybe String)
clGetDeviceProfile = getDeviceInfoString . getCLValue $ CL_DEVICE_PROFILE

-- | Describes the resolution of device timer. This is measured in nanoseconds.
clGetDeviceProfilingTimerResolution :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceProfilingTimerResolution = getDeviceInfoSizet . getCLValue $ CL_DEVICE_PROFILING_TIMER_RESOLUTION

-- | Describes the command-queue properties supported by the device. This is a 
-- list that describes one or more of the CLCommandQueueProperty values.
-- These properties are described in the table for 'clCreateCommandQueue'. 
-- The mandated minimum capability is 'CL_QUEUE_PROFILING_ENABLE'.
clGetDeviceQueueProperties :: CLDeviceID -> IO [CLCommandQueueProperty]
clGetDeviceQueueProperties device = fmap (bitmaskToCommandQueueProperties . fromMaybe 0) $ getDeviceInfoUlong (getCLValue CL_DEVICE_QUEUE_PROPERTIES) device

-- | Describes single precision floating-point capability of the device. This is 
-- a bit-field that describes one or more of the 'CLDeviceFPConfig' values.
-- The mandated minimum floating-point capability is 'CL_FP_ROUND_TO_NEAREST' | 
-- 'CL_FP_INF_NAN'.
clGetDeviceSingleFPConfig :: CLDeviceID -> IO [CLDeviceFPConfig]
clGetDeviceSingleFPConfig = getDeviceInfoFP . getCLValue $ CL_DEVICE_SINGLE_FP_CONFIG

-- | The OpenCL device type. Currently supported values are one of or a 
-- combination of: 'CL_DEVICE_TYPE_CPU', 'CL_DEVICE_TYPE_GPU', 
-- 'CL_DEVICE_TYPE_ACCELERATOR', or 'CL_DEVICE_TYPE_DEFAULT'.
clGetDeviceType :: CLDeviceID -> IO [CLDeviceType]
clGetDeviceType device = fmap (bitmaskToDeviceTypes . fromMaybe 0) $ getDeviceInfoUlong (getCLValue CL_DEVICE_TYPE) device

-- | Vendor name string.
clGetDeviceVendor :: CLDeviceID -> IO (Maybe String)
clGetDeviceVendor = getDeviceInfoString . getCLValue $ CL_DEVICE_VENDOR

-- | A unique device vendor identifier. An example of a unique device identifier 
-- could be the PCIe ID.
clGetDeviceVendorID :: CLDeviceID -> IO (Maybe CLuint)
clGetDeviceVendorID = getDeviceInfoUint . getCLValue $ CL_DEVICE_VENDOR_ID

-- | OpenCL version string. Returns the OpenCL version supported by the device. 
-- This version string has the following format:
-- /OpenCL major_version.minor_version vendor-specific information/
-- The major_version.minor_version value returned will be 1.0.
clGetDeviceVersion :: CLDeviceID -> IO (Maybe String)
clGetDeviceVersion = getDeviceInfoString . getCLValue $ CL_DEVICE_VERSION

-- | OpenCL software driver version string in the form major_number.minor_number.
clGetDeviceDriverVersion :: CLDeviceID -> IO (Maybe String)
clGetDeviceDriverVersion = getDeviceInfoString . getCLValue $ CL_DRIVER_VERSION

-- -----------------------------------------------------------------------------