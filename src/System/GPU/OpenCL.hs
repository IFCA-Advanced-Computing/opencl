-- -----------------------------------------------------------------------------
-- This file is part of Skema.

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
module System.GPU.OpenCL( 
  -- * Types
  CLPlatformInfo(..), CLDeviceType(..), CLPlatformID, CLDeviceID,
  -- * Functions
  clGetPlatformIDs, clGetPlatformInfo, 
  clGetDeviceIDs ) 
       where

-- -----------------------------------------------------------------------------
import Data.Bits( shiftL, complement )
import Data.Maybe( fromMaybe )
import Foreign( Ptr, nullPtr, castPtr, alloca, allocaArray, peek, peekArray )
import Foreign.C.Types( CSize, CULong, CUInt )
import Foreign.C.String( CString, peekCString )
import Foreign.Storable( sizeOf )
import System.GPU.OpenCL.Types( CLPlatformID, CLDeviceID, CLuint, CLint )
import System.GPU.OpenCL.Errors( ErrorCode(..), clSuccess )

-- -----------------------------------------------------------------------------
foreign import ccall "clGetPlatformIDs" raw_clGetPlatformIDs :: 
  CLuint -> Ptr CLPlatformID -> Ptr CLuint -> IO CLint
foreign import ccall "clGetPlatformInfo" raw_clGetPlatformInfo :: 
  CLPlatformID -> CLuint -> CSize -> Ptr () -> Ptr CSize -> IO CLint 
foreign import ccall "clGetDeviceIDs" raw_clGetDeviceIDs :: 
  CLPlatformID -> CULong -> CLuint -> Ptr CLDeviceID -> Ptr CLuint -> IO CLint
foreign import ccall "clGetDeviceInfo" raw_clGetDeviceInfo :: 
  CLDeviceID -> CLuint -> CSize -> Ptr () -> Ptr CSize -> IO CLint

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
  
data CLPlatformInfo = CL_PLATFORM_PROFILE 
                      -- ^ OpenCL profile string. Returns the profile name 
                      -- supported by the implementation. The profile name 
                      -- returned can be one of the following strings:
                      --
                      --  * @FULL_PROFILE@ - if the implementation supports the 
                      -- OpenCL specification (functionality defined as part of 
                      -- the core specification and does not require any 
                      -- extensions to be supported).
                      --
                      --  * @EMBEDDED_PROFILE@ - if the implementation supports the 
                      -- OpenCL embedded profile. The embedded profile is 
                      -- defined to be a subset for each version of OpenCL.
                    | CL_PLATFORM_VERSION 
                      -- ^ OpenCL version string. Returns the OpenCL version 
                      -- supported by the implementation. This version string 
                      -- has the following format:
                      -- /OpenCL major_version.minor_version platform-specific information/
                      -- The /major_version.minor_version/ value returned will 
                      -- be 1.0.
                    | CL_PLATFORM_NAME -- ^ Platform name string.
                    | CL_PLATFORM_VENDOR -- ^ Platform vendor string.
                    | CL_PLATFORM_EXTENSIONS 
                      -- ^ Returns a space-separated list of extension names 
                      -- (the extension names themselves do not contain any 
                      -- spaces) supported by the platform. Extensions 
                      -- defined here must be supported by all devices 
                      -- associated with this platform.
                    deriving( Eq )

platformInfoValues :: [(CLPlatformInfo,CLuint)]
platformInfoValues = [ 
  (CL_PLATFORM_PROFILE,0x0900), (CL_PLATFORM_VERSION,0x0901), 
  (CL_PLATFORM_NAME,0x0902), (CL_PLATFORM_VENDOR,0x0903), 
  (CL_PLATFORM_EXTENSIONS,0x0904) ]
getPlatformInfoValue :: CLPlatformInfo -> CLuint
getPlatformInfoValue info = fromMaybe 0 (lookup info platformInfoValues)

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
      infoval = getPlatformInfoValue infoid

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
                  deriving( Eq )

deviceTypeValues :: [(CLDeviceType,CULong)]
deviceTypeValues = [ 
  (CL_DEVICE_TYPE_CPU, 1 `shiftL` 1), (CL_DEVICE_TYPE_GPU, 1 `shiftL` 2), 
  (CL_DEVICE_TYPE_ACCELERATOR, 1 `shiftL` 3), (CL_DEVICE_TYPE_DEFAULT, 1 `shiftL` 0),
  (CL_DEVICE_TYPE_ALL, complement 0) ]
getDeviceTypeValue :: CLDeviceType -> CULong
getDeviceTypeValue info = fromMaybe 0 (lookup info deviceTypeValues)

getNumDevices :: CLPlatformID -> CULong -> IO (Maybe CLuint)
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
      dval = getDeviceTypeValue dtype

{-
CL_DEVICE_ADDRESS_BITS	
Return type: cl_uint

The default compute device address space size specified as an unsigned integer value in bits. Currently supported values are 32 or 64 bits.

CL_DEVICE_AVAILABLE	
Return type: cl_bool

Is CL_TRUE if the device is available and CL_FALSE if the device is not available.

CL_DEVICE_COMPILER_AVAILABLE	
Return type: cl_bool

Is CL_FALSE if the implementation does not have a compiler available to compile the program source. Is CL_TRUE if the compiler is available. This can be CL_FALSE for the embededed platform profile only.

CL_DEVICE_DOUBLE_FP_CONFIG	
Return type: cl_device_fp_config

Describes the OPTIONAL double precision floating-point capability of the OpenCL device. This is a bit-field that describes one or more of the following values:

CL_FP_DENORM - denorms are supported.
CL_FP_INF_NAN - INF and NaNs are supported.
CL_FP_ROUND_TO_NEAREST - round to nearest even rounding mode supported.
CL_FP_ROUND_TO_ZERO - round to zero rounding mode supported.
CL_FP_ROUND_TO_INF - round to +ve and -ve infinity rounding modes supported.
CP_FP_FMA - IEEE754-2008 fused multiply-add is supported.
The mandated minimum double precision floating-point capability is CL_FP_FMA | CL_FP_ROUND_TO_NEAREST | CL_FP_ROUND_TO_ZERO | CL_FP_ROUND_TO_INF | CL_FP_INF_NAN | CL_FP_DENORM.

CL_DEVICE_ENDIAN_LITTLE	
Return type: cl_bool

Is CL_TRUE if the OpenCL device is a little endian device and CL_FALSE otherwise.

CL_DEVICE_ERROR_CORRECTION_SUPPORT	
Return type: cl_bool

Is CL_TRUE if the device implements error correction for the memories, caches, registers etc. in the device. Is CL_FALSE if the device does not implement error correction. This can be a requirement for certain clients of OpenCL.

CL_DEVICE_EXECUTION_CAPABILITIES	
Return type: cl_device_exec_capabilities

Describes the execution capabilities of the device. This is a bit-field that describes one or more of the following values:

CL_EXEC_KERNEL - The OpenCL device can execute OpenCL kernels.

CL_EXEC_NATIVE_KERNEL - The OpenCL device can execute native kernels.

The mandated minimum capability is CL_EXEC_KERNEL.

CL_DEVICE_EXTENSIONS	
Return type: char[]

Returns a space separated list of extension names (the extension names themselves do not contain any spaces). The list of extension names returned currently can include one or more of the following approved extension names:

cl_khr_fp64
cl_khr_select_fprounding_mode
cl_khr_global_int32_base_atomics
cl_khr_global_int32_extended_atomics
cl_khr_local_int32_base_atomics
cl_khr_local_int32_extended_atomics
cl_khr_int64_base_atomics
cl_khr_int64_extended_atomics
cl_khr_3d_image_writes
cl_khr_byte_addressable_store
cl_khr_fp16

CL_DEVICE_GLOBAL_MEM_CACHE_SIZE	
Return type: cl_ulong

Size of global memory cache in bytes.

CL_DEVICE_GLOBAL_MEM_CACHE_TYPE	
Return type: cl_device_mem_cache_type

Type of global memory cache supported. Valid values are: CL_NONE, CL_READ_ONLY_CACHE, and CL_READ_WRITE_CACHE.

CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE	
Return type: cl_uint

Size of global memory cache line in bytes.

CL_DEVICE_GLOBAL_MEM_SIZE	
Return type: cl_ulong

Size of global device memory in bytes.

CL_DEVICE_HALF_FP_CONFIG	
Return type: cl_device_fp_config

Describes the OPTIONAL half precision floating-point capability of the OpenCL device. This is a bit-field that describes one or more of the following values:

CL_FP_DENORM - denorms are supported.
CL_FP_INF_NAN - INF and NaNs are supported.
CL_FP_ROUND_TO_NEAREST - round to nearest even rounding mode supported.
CL_FP_ROUND_TO_ZERO - round to zero rounding mode supported.
CL_FP_ROUND_TO_INF - round to +ve and -ve infinity rounding modes supported.
CP_FP_FMA - IEEE754-2008 fused multiply-add is supported.
The required minimum half precision floating-point capability as implemented by this extension is CL_FP_ROUND_TO_ZERO | CL_FP_ROUND_TO_INF | CL_FP_INF_NAN.

CL_DEVICE_IMAGE_SUPPORT	
Return type: cl_bool

Is CL_TRUE if images are supported by the OpenCL device and CL_FALSE otherwise.

CL_DEVICE_IMAGE2D_MAX_HEIGHT	
Return type: size_t

Max height of 2D image in pixels. The minimum value is 8192 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE.

CL_DEVICE_IMAGE2D_MAX_WIDTH	
Return type: size_t

Max width of 2D image in pixels. The minimum value is 8192 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE.

CL_DEVICE_IMAGE3D_MAX_DEPTH	
Return type: size_t

Max depth of 3D image in pixels. The minimum value is 2048 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE.

CL_DEVICE_IMAGE3D_MAX_HEIGHT	
Return type: size_t

Max height of 3D image in pixels. The minimum value is 2048 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE.

CL_DEVICE_IMAGE3D_MAX_WIDTH	
Return type: size_t

Max width of 3D image in pixels. The minimum value is 2048 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE.

CL_DEVICE_LOCAL_MEM_SIZE	
Return type: cl_ulong

Size of local memory arena in bytes. The minimum value is 16 KB.

CL_DEVICE_LOCAL_MEM_TYPE	
Return type: cl_device_local_mem_type

Type of local memory supported. This can be set to CL_LOCAL implying dedicated local memory storage such as SRAM, or CL_GLOBAL.

CL_DEVICE_MAX_CLOCK_FREQUENCY	
Return type: cl_uint

Maximum configured clock frequency of the device in MHz.

CL_DEVICE_MAX_COMPUTE_UNITS	
Return type: cl_uint

The number of parallel compute cores on the OpenCL device. The minimum value is 1.

CL_DEVICE_MAX_CONSTANT_ARGS	
Return type: cl_uint

Max number of arguments declared with the __constant qualifier in a kernel. The minimum value is 8.

CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE	
Return type: cl_ulong

Max size in bytes of a constant buffer allocation. The minimum value is 64 KB.

CL_DEVICE_MAX_MEM_ALLOC_SIZE	
Return type: cl_ulong

Max size of memory object allocation in bytes. The minimum value is max (1/4th of CL_DEVICE_GLOBAL_MEM_SIZE, 128*1024*1024)

CL_DEVICE_MAX_PARAMETER_SIZE	
Return type: size_t

Max size in bytes of the arguments that can be passed to a kernel. The minimum value is 256.

CL_DEVICE_MAX_READ_IMAGE_ARGS	
Return type: cl_uint

Max number of simultaneous image objects that can be read by a kernel. The minimum value is 128 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE.

CL_DEVICE_MAX_SAMPLERS	
Return type: cl_uint

Maximum number of samplers that can be used in a kernel. The minimum value is 16 if CL_DEVICE_IMAGE_SUPPORT is CL_TRUE. (Also see sampler_t.)

CL_DEVICE_MAX_WORK_GROUP_SIZE	
Return type: size_t

Maximum number of work-items in a work-group executing a kernel using the data parallel execution model. (Refer to clEnqueueNDRangeKernel). The minimum value is 1.
-}

getDeviceInfoSize :: CLDeviceID -> CLuint -> IO (Maybe CSize)
getDeviceInfoSize device infoid = alloca $ \(value_size :: Ptr CSize) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid 0 nullPtr value_size
  if errcode == clSuccess
    then fmap Just $ peek value_size
    else return Nothing
  
getDeviceInfoString :: CLuint -> CLDeviceID -> IO (Maybe String)
getDeviceInfoString infoid device = do
  sval <- getDeviceInfoSize device infoid
  case sval of
    Nothing -> return Nothing
    Just n -> allocaArray (fromIntegral n) $ \(buff :: CString) -> do
      errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid n (castPtr buff) nullPtr
      if errcode == clSuccess
        then fmap Just $ peekCString buff
        else return Nothing
  
getDeviceInfoUint :: CLuint -> CLDeviceID -> IO (Maybe CUInt)
getDeviceInfoUint infoid device = alloca $ \(dat :: Ptr CUInt) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CUInt)

getDeviceInfoSizet :: CLuint -> CLDeviceID -> IO (Maybe CSize)
getDeviceInfoSizet infoid device = alloca $ \(dat :: Ptr CSize) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CSize)
  
-- clDeviceType = DeviceInfo 0x1000 
-- clDeviceMaxComputeUnits = DeviceInfo 0x1002
-- clDeviceMaxWorkGroupSize = DeviceInfo 0x1004
-- clDeviceMaxWorkItemSizes = DeviceInfo 0x1005
-- clDeviceMaxClockFrequency = DeviceInfo 0x100C
-- clDeviceAddressBits = DeviceInfo 0x100D
-- clDeviceMaxReadImageArgs = DeviceInfo 0x100E 
-- clDeviceMaxMemAllocSize = DeviceInfo 0x1010
-- clDeviceImage2DMaxWidth = DeviceInfo 0x1011
-- clDeviceImage2DMaxHeight = DeviceInfo 0x1012
-- clDeviceImage3DMaxWidth = DeviceInfo 0x1013
-- clDeviceImage3DMaxHeight = DeviceInfo 0x1014
-- clDeviceImage3DMaxDepth = DeviceInfo 0x1015
-- clDeviceImageSupport = DeviceInfo 0x1016
-- clDeviceMaxParameterSize = DeviceInfo 0x1017
-- clDeviceMaxSamplers = DeviceInfo 0x1018
-- clDeviceSingleFPConfig = DeviceInfo 0x101B
-- clDeviceGlobalMemCacheType = DeviceInfo 0x101C
-- clDeviceGlobalMemCacheLineSize = DeviceInfo 0x101D
-- clDeviceGlobalMemCacheSize = DeviceInfo 0x101E
-- clDeviceGlobalMemSize = DeviceInfo 0x101F
-- clDeviceMaxConstantBuffersize = DeviceInfo 0x1020
-- clDeviceMaxConstantArgs = DeviceInfo 0x1021
-- clDeviceMLocalMemType = DeviceInfo 0x1022
-- clDeviceLocalMemSize = DeviceInfo 0x1023
-- clDeviceErrorCorrectionSupport = DeviceInfo 0x1024
-- clDeviceEndianLittle = DeviceInfo 0x1026
-- clDeviceAvailable = DeviceInfo 0x1027
-- clDeviceCompilerAvailable = DeviceInfo 0x1028
-- clDeviceExecutionCapabilities = DeviceInfo 0x1029
-- clDeviceQueueProperties = DeviceInfo 0x102A
-- clDeviceName = DeviceInfo 0x102B
-- clDeviceExtensions = DeviceInfo 0x1030

-- | Maximum dimensions that specify the global and local work-item IDs used by 
-- the data parallel execution model. (Refer to clEnqueueNDRangeKernel). 
-- The minimum value is 3.
clGetDeviceMaxWorkItemDimensions :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceMaxWorkItemDimensions = getDeviceInfoUint 0x1003

--CL_DEVICE_MAX_WORK_ITEM_SIZES	
--Return type: size_t[]
-- | Maximum number of work-items that can be specified in each dimension of the 
-- work-group to clEnqueueNDRangeKernel.
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
      infoid = 0x1005
      size n = fromIntegral $ (fromIntegral n) * (sizeOf (0::CSize))

-- | Max number of simultaneous image objects that can be written to by a 
-- kernel. The minimum value is 8 if 'clGetDeviceImageSupport' is 'True'.
clGetDeviceMaxWriteImageArgs :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceMaxWriteImageArgs = getDeviceInfoUint 0x100F

-- | Describes the alignment in bits of the base address of any allocated 
-- memory object.
clGetDeviceMemBaseAddrAlign :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceMemBaseAddrAlign = getDeviceInfoUint 0x1019

-- | The smallest alignment in bytes which can be used for any data type.
clGetDeviceMinDataTypeAlignSize :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceMinDataTypeAlignSize = getDeviceInfoUint 0x101A

-- | Device name string.
clGetDeviceName :: CLDeviceID -> IO (Maybe String)
clGetDeviceName = getDeviceInfoString 0x102B

-- | The platform associated with this device.
clGetDevicePlatform :: CLDeviceID -> IO (Maybe CLPlatformID)
clGetDevicePlatform device = alloca $ \(dat :: Ptr CLPlatformID) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      infoid = 0x1031
      size = fromIntegral $ sizeOf (nullPtr :: CLPlatformID)

-- | Preferred native vector width size for built-in char types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
clGetDevicePreferredVectorWidthChar :: CLDeviceID -> IO (Maybe CUInt)
clGetDevicePreferredVectorWidthChar = getDeviceInfoUint 0x1006

-- | Preferred native vector width size for built-in short types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
clGetDevicePreferredVectorWidthShort :: CLDeviceID -> IO (Maybe CUInt)
clGetDevicePreferredVectorWidthShort = getDeviceInfoUint 0x1007

-- | Preferred native vector width size for built-in int types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
clGetDevicePreferredVectorWidthInt :: CLDeviceID -> IO (Maybe CUInt)
clGetDevicePreferredVectorWidthInt = getDeviceInfoUint 0x1008

-- | Preferred native vector width size for built-in long types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
clGetDevicePreferredVectorWidthLong :: CLDeviceID -> IO (Maybe CUInt)
clGetDevicePreferredVectorWidthLong = getDeviceInfoUint 0x1009

-- | Preferred native vector width size for built-in float types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
clGetDevicePreferredVectorWidthFloat :: CLDeviceID -> IO (Maybe CUInt)
clGetDevicePreferredVectorWidthFloat = getDeviceInfoUint 0x100A

-- | Preferred native vector width size for built-in double types that can be put 
-- into vectors. The vector width is defined as the number of scalar elements 
-- that can be stored in the vector.
-- | If the cl_khr_fp64 extension is not supported, 
-- 'clGetDevicePreferredVectorWidthDouble' must return 0.
clGetDevicePreferredVectorWidthDouble :: CLDeviceID -> IO (Maybe CUInt)
clGetDevicePreferredVectorWidthDouble = getDeviceInfoUint 0x100B

-- | OpenCL profile string. Returns the profile name supported by the device 
-- (see note). The profile name returned can be one of the following strings:
-- * FULL_PROFILE - if the device supports the OpenCL specification 
-- (functionality defined as part of the core specification and does not require 
-- any extensions to be supported).
-- * EMBEDDED_PROFILE - if the device supports the OpenCL embedded profile.
clGetDeviceProfile :: CLDeviceID -> IO (Maybe String)
clGetDeviceProfile = getDeviceInfoString 0x102E

-- | Describes the resolution of device timer. This is measured in nanoseconds.
clGetDeviceProfilingTimerResolution :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceProfilingTimerResolution = getDeviceInfoSizet 0x1025

{-
CL_DEVICE_QUEUE_PROPERTIES	
Return type: cl_command_queue_properties

Describes the command-queue properties supported by the device. This is a bit-field that describes one or more of the following values:

CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE

CL_QUEUE_PROFILING_ENABLE

These properties are described in the table for clCreateCommandQueue. The mandated minimum capability is CL_QUEUE_PROFILING_ENABLE.

CL_DEVICE_SINGLE_FP_CONFIG	
Return type: cl_device_fp_config

Describes single precision floating-point capability of the device. This is a bit-field that describes one or more of the following values:

CL_FP_DENORM - denorms are supported
CL_FP_INF_NAN - INF and quiet NaNs are supported
CL_FP_ROUND_TO_NEAREST - round to nearest even rounding mode supported
CL_FP_ROUND_TO_ZERO - round to zero rounding mode supported
CL_FP_ROUND_TO_INF - round to +ve and -ve infinity rounding modes supported
CL_FP_FMA - IEEE754-2008 fused multiply-add is supported

The mandated minimum floating-point capability is CL_FP_ROUND_TO_NEAREST | CL_FP_INF_NAN.

CL_DEVICE_TYPE	
Return type: cl_device_type

The OpenCL device type. Currently supported values are one of or a combination of: CL_DEVICE_TYPE_CPU, CL_DEVICE_TYPE_GPU, CL_DEVICE_TYPE_ACCELERATOR, or CL_DEVICE_TYPE_DEFAULT.

-}

-- | Vendor name string.
clGetDeviceVendor :: CLDeviceID -> IO (Maybe String)
clGetDeviceVendor = getDeviceInfoString 0x102C

-- | A unique device vendor identifier. An example of a unique device identifier 
-- could be the PCIe ID.
clGetDeviceVendorID :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceVendorID = getDeviceInfoUint 0x1001

-- | OpenCL version string. Returns the OpenCL version supported by the device. 
-- This version string has the following format:
-- /OpenCL major_version.minor_version vendor-specific information/
-- The major_version.minor_version value returned will be 1.0.
clGetDeviceVersion :: CLDeviceID -> IO (Maybe String)
clGetDeviceVersion = getDeviceInfoString 0x102F

-- | OpenCL software driver version string in the form major_number.minor_number.
clGetDeviceDriverVersion :: CLDeviceID -> IO (Maybe String)
clGetDeviceDriverVersion = getDeviceInfoString 0x102D

-- -----------------------------------------------------------------------------
