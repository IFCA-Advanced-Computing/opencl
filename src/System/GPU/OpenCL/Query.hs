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
  CLPlatformInfo(..), CLPlatformID, CLDeviceID, 
  CLDeviceFPConfig(..), CLDeviceExecCapability(..), CLDeviceLocalMemType(..),
  CLDeviceMemCacheType(..), CLCommandQueueProperty(..),
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
import Data.Bits( shiftL )
import Data.Maybe( fromMaybe )
import Foreign( Ptr, nullPtr, castPtr, alloca, allocaArray, peek, peekArray )
import Foreign.C.Types( CSize, CULong, CUInt )
import Foreign.C.String( CString, peekCString )
import Foreign.Storable( sizeOf )
import System.GPU.OpenCL.Types( CLPlatformID, CLDeviceID, CLuint, CLint, CLDeviceType(..), getDeviceTypeValue, bitmaskToDeviceTypes )
import System.GPU.OpenCL.Errors( ErrorCode(..), clSuccess )
import System.GPU.OpenCL.Util( testMask )

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

getDeviceInfoUlong :: CLuint -> CLDeviceID -> IO (Maybe CULong)
getDeviceInfoUlong infoid device = alloca $ \(dat :: Ptr CULong) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CULong)

getDeviceInfoSizet :: CLuint -> CLDeviceID -> IO (Maybe CSize)
getDeviceInfoSizet infoid device = alloca $ \(dat :: Ptr CSize) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CSize)
  
getDeviceInfoBool :: CLuint -> CLDeviceID -> IO (Maybe Bool)
getDeviceInfoBool infoid device = alloca $ \(dat :: Ptr CUInt) -> do
  errcode <- fmap ErrorCode $ raw_clGetDeviceInfo device infoid size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap (Just . (/=0)) $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CUInt)
  
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
                        
deviceFPValues :: [(CLDeviceFPConfig,CULong)]
deviceFPValues = [
  (CL_FP_DENORM, 1 `shiftL` 0), (CL_FP_INF_NAN, 1 `shiftL` 1),
  (CL_FP_ROUND_TO_NEAREST, 1 `shiftL` 2), (CL_FP_ROUND_TO_ZERO, 1 `shiftL` 3),
  (CL_FP_ROUND_TO_INF, 1 `shiftL` 4), (CL_FP_FMA, 1 `shiftL` 5)]

data CLDeviceExecCapability = CL_EXEC_KERNEL 
                              -- ^ The OpenCL device can execute OpenCL kernels.
                            | CL_EXEC_NATIVE_KERNEL
                              -- ^ The OpenCL device can execute native kernels.
                              deriving( Show )

deviceExecValues :: [(CLDeviceExecCapability,CULong)]
deviceExecValues = [
  (CL_EXEC_KERNEL, 1 `shiftL` 0), (CL_EXEC_NATIVE_KERNEL, 1 `shiftL` 1)]
                   
data CLCommandQueueProperty = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE
                            | CL_QUEUE_PROFILING_ENABLE
                              deriving( Show )
                                      
commandQueueProperties :: [(CLCommandQueueProperty,CULong)]                                      
commandQueueProperties = [
  (CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE, 1 `shiftL` 0),
  (CL_QUEUE_PROFILING_ENABLE, 1 `shiftL` 1)]

bitmaskToFPConfig :: CULong -> [CLDeviceFPConfig]
bitmaskToFPConfig mask = map fst . filter (testMask mask) $ deviceFPValues

bitmaskToExecCapability :: CULong -> [CLDeviceExecCapability]
bitmaskToExecCapability mask = map fst . filter (testMask mask) $ deviceExecValues

bitmaskToCommandQueueProperties :: CULong -> [CLCommandQueueProperty]
bitmaskToCommandQueueProperties mask = map fst . filter (testMask mask) $ commandQueueProperties

getDeviceInfoFP :: CLuint -> CLDeviceID -> IO [CLDeviceFPConfig]
getDeviceInfoFP infoid device = fmap (bitmaskToFPConfig . fromMaybe 0) $ getDeviceInfoUlong infoid device

-- | The default compute device address space size specified as an unsigned 
-- integer value in bits. Currently supported values are 32 or 64 bits.
clGetDeviceAddressBits :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceAddressBits = getDeviceInfoUint 0x100D

-- | Is 'True' if the device is available and 'False' if the device is not 
-- available.
clGetDeviceAvailable :: CLDeviceID -> IO (Maybe Bool)
clGetDeviceAvailable = getDeviceInfoBool 0x1027

-- | Is 'False' if the implementation does not have a compiler available to 
-- compile the program source. Is 'True' if the compiler is available. This can 
-- be 'False' for the embededed platform profile only.
clGetDeviceCompilerAvailable :: CLDeviceID -> IO (Maybe Bool)
clGetDeviceCompilerAvailable = getDeviceInfoBool 0x1028

-- | Describes the OPTIONAL double precision floating-point capability of the 
-- OpenCL device. This is a bit-field that describes one or more of the 
-- 'CLDeviceFPConfig' values.
-- The mandated minimum double precision floating-point capability is 
-- 'CL_FP_FMA' | 'CL_FP_ROUND_TO_NEAREST' | 'CL_FP_ROUND_TO_ZERO' | 
-- 'CL_FP_ROUND_TO_INF' | 'CL_FP_INF_NAN' | 'CL_FP_DENORM'.
clGetDeviceDoubleFPConfig :: CLDeviceID -> IO [CLDeviceFPConfig]
clGetDeviceDoubleFPConfig = getDeviceInfoFP 0x1032

-- | Is 'True' if the OpenCL device is a little endian device and 'False' 
-- otherwise.
clGetDeviceEndianLittle :: CLDeviceID -> IO (Maybe Bool)
clGetDeviceEndianLittle = getDeviceInfoBool 0x1026

-- | Is 'True' if the device implements error correction for the memories, 
-- caches, registers etc. in the device. Is 'False' if the device does not 
-- implement error correction. This can be a requirement for certain clients of 
-- OpenCL.
clGetDeviceErrorCorrectionSupport :: CLDeviceID -> IO (Maybe Bool)
clGetDeviceErrorCorrectionSupport = getDeviceInfoBool 0x1024

-- | Describes the execution capabilities of the device. This is a list that 
-- describes one or more of the 'CLDeviceExecCapability' values.
-- The mandated minimum capability is 'CL_EXEC_KERNEL'.
clGetDeviceExecutionCapabilities :: CLDeviceID -> IO [CLDeviceExecCapability]
clGetDeviceExecutionCapabilities device = fmap (bitmaskToExecCapability . fromMaybe 0) $ getDeviceInfoUlong 0x1029 device

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
clGetDeviceExtensions = getDeviceInfoString 0x1030

-- | Size of global memory cache in bytes.
clGetDeviceGlobalMemCacheSize :: CLDeviceID -> IO (Maybe CULong)
clGetDeviceGlobalMemCacheSize = getDeviceInfoUlong 0x101E

data CLDeviceMemCacheType = CL_NONE | CL_READ_ONLY_CACHE | CL_READ_WRITE_CACHE
                          deriving( Show )
deviceMemCacheTypes :: [(CUInt,CLDeviceMemCacheType)]
deviceMemCacheTypes = [
  (0x0,CL_NONE), (0x1,CL_READ_ONLY_CACHE),(0x2,CL_READ_WRITE_CACHE)]
getDeviceMemCacheType :: CUInt -> Maybe CLDeviceMemCacheType
getDeviceMemCacheType val = lookup val deviceMemCacheTypes

-- | Type of global memory cache supported. Valid values are: 'CL_NONE', 
-- 'CL_READ_ONLY_CACHE', and 'CL_READ_WRITE_CACHE'.
clGetDeviceGlobalMemCacheType :: CLDeviceID -> IO (Maybe CLDeviceMemCacheType)
clGetDeviceGlobalMemCacheType device = getDeviceInfoUint 0x101C device >>= return . maybe Nothing getDeviceMemCacheType

-- | Size of global memory cache line in bytes.
clGetDeviceGlobalMemCachelineSize :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceGlobalMemCachelineSize = getDeviceInfoUint 0x101D

-- | Size of global device memory in bytes.
clGetDeviceGlobalMemSize :: CLDeviceID -> IO (Maybe CULong)
clGetDeviceGlobalMemSize = getDeviceInfoUlong 0x101F

-- | Describes the OPTIONAL half precision floating-point capability of the 
-- OpenCL device. This is a bit-field that describes one or more of the 
-- 'CLDeviceFPConfig' values.
-- The required minimum half precision floating-point capability as implemented 
-- by this extension is 'CL_FP_ROUND_TO_ZERO' | 'CL_FP_ROUND_TO_INF' | 
-- 'CL_FP_INF_NAN'.
clGetDeviceHalfFPConfig :: CLDeviceID -> IO [CLDeviceFPConfig]
clGetDeviceHalfFPConfig = getDeviceInfoFP 0x1033

-- | Is 'True' if images are supported by the OpenCL device and 'False' otherwise.
clGetDeviceImageSupport :: CLDeviceID -> IO (Maybe Bool)
clGetDeviceImageSupport = getDeviceInfoBool 0x1016

-- | Max height of 2D image in pixels. The minimum value is 8192 if
-- 'clGetDeviceImageSupport' is 'True'.
clGetDeviceImage2DMaxHeight :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceImage2DMaxHeight = getDeviceInfoSizet 0x1012

-- | Max width of 2D image in pixels. The minimum value is 8192 if
-- 'clGetDeviceImageSupport' is 'True'.
clGetDeviceImage2DMaxWidth :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceImage2DMaxWidth = getDeviceInfoSizet 0x1011

-- | Max depth of 3D image in pixels. The minimum value is 2048 if 
-- 'clGetDeviceImageSupport' is 'True'.
clGetDeviceImage3DMaxDepth :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceImage3DMaxDepth = getDeviceInfoSizet 0x1015

-- | Max height of 3D image in pixels. The minimum value is 2048 if 
-- 'clGetDeviceImageSupport' is 'True'.
clGetDeviceImage3DMaxHeight :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceImage3DMaxHeight = getDeviceInfoSizet 0x1014

-- | Max width of 3D image in pixels. The minimum value is 2048 if 
-- 'clGetDeviceImageSupport' is 'True'.
clGetDeviceImage3DMaxWidth :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceImage3DMaxWidth = getDeviceInfoSizet 0x1013

-- | Size of local memory arena in bytes. The minimum value is 16 KB.
clGetDeviceLocalMemSize :: CLDeviceID -> IO (Maybe CULong)
clGetDeviceLocalMemSize = getDeviceInfoUlong 0x1023

data CLDeviceLocalMemType = CL_LOCAL | CL_GLOBAL deriving( Show )

deviceLocalMemTypes :: [(CUInt,CLDeviceLocalMemType)]
deviceLocalMemTypes = [(0x1,CL_LOCAL), (0x2,CL_GLOBAL)]
getDeviceLocalMemType :: CUInt -> Maybe CLDeviceLocalMemType
getDeviceLocalMemType val = lookup val deviceLocalMemTypes

-- | Type of local memory supported. This can be set to 'CL_LOCAL' implying 
-- dedicated local memory storage such as SRAM, or 'CL_GLOBAL'.
clGetDeviceLocalMemType :: CLDeviceID -> IO (Maybe CLDeviceLocalMemType)
clGetDeviceLocalMemType device = getDeviceInfoUint 0x1022 device >>= return . maybe Nothing getDeviceLocalMemType

-- | Maximum configured clock frequency of the device in MHz.
clGetDeviceMaxClockFrequency :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceMaxClockFrequency = getDeviceInfoUint 0x100C

-- | The number of parallel compute cores on the OpenCL device. The minimum 
-- value is 1.
clGetDeviceMaxComputeUnits :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceMaxComputeUnits = getDeviceInfoUint 0x1002

-- | Max number of arguments declared with the __constant qualifier in a kernel. 
-- The minimum value is 8.
clGetDeviceMaxConstantArgs :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceMaxConstantArgs = getDeviceInfoUint 0x1021

-- | Max size in bytes of a constant buffer allocation. The minimum value is 
-- 64 KB.
clGetDeviceMaxConstantBufferSize :: CLDeviceID -> IO (Maybe CULong)
clGetDeviceMaxConstantBufferSize = getDeviceInfoUlong 0x1020

-- | Max size of memory object allocation in bytes. The minimum value is max 
-- (1/4th of 'clGetDeviceGlobalMemSize', 128*1024*1024)
clGetDeviceMaxMemAllocSize :: CLDeviceID -> IO (Maybe CULong)
clGetDeviceMaxMemAllocSize = getDeviceInfoUlong 0x1010

-- | Max size in bytes of the arguments that can be passed to a kernel. The 
-- minimum value is 256.
clGetDeviceMaxParameterSize :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceMaxParameterSize = getDeviceInfoSizet 0x1017

-- | Max number of simultaneous image objects that can be read by a kernel. The 
-- minimum value is 128 if 'clGetDeviceImageSupport' is 'True'.
clGetDeviceMaxReadImageArgs :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceMaxReadImageArgs = getDeviceInfoUint 0x100E

-- | Maximum number of samplers that can be used in a kernel. The minimum value 
-- is 16 if 'clGetDeviceImageSupport' is 'True'. (Also see sampler type.)
clGetDeviceMaxSamplers :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceMaxSamplers = getDeviceInfoUint 0x1018

-- | Maximum number of work-items in a work-group executing a kernel using the 
-- data parallel execution model. (Refer to 'clEnqueueNDRangeKernel'). The 
-- minimum value is 1.
clGetDeviceMaxWorkGroupSize :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceMaxWorkGroupSize = getDeviceInfoSizet 0x1004

-- | Maximum dimensions that specify the global and local work-item IDs used by 
-- the data parallel execution model. (Refer to 'clEnqueueNDRangeKernel'). 
-- The minimum value is 3.
clGetDeviceMaxWorkItemDimensions :: CLDeviceID -> IO (Maybe CUInt)
clGetDeviceMaxWorkItemDimensions = getDeviceInfoUint 0x1003

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
-- 
-- * FULL_PROFILE - if the device supports the OpenCL specification 
-- (functionality defined as part of the core specification and does not require 
-- any extensions to be supported).
-- 
-- * EMBEDDED_PROFILE - if the device supports the OpenCL embedded profile.
clGetDeviceProfile :: CLDeviceID -> IO (Maybe String)
clGetDeviceProfile = getDeviceInfoString 0x102E

-- | Describes the resolution of device timer. This is measured in nanoseconds.
clGetDeviceProfilingTimerResolution :: CLDeviceID -> IO (Maybe CSize)
clGetDeviceProfilingTimerResolution = getDeviceInfoSizet 0x1025

-- | Describes the command-queue properties supported by the device. This is a 
-- list that describes one or more of the CLCommandQueueProperty values.
-- These properties are described in the table for 'clCreateCommandQueue'. 
-- The mandated minimum capability is 'CL_QUEUE_PROFILING_ENABLE'.
clGetDeviceQueueProperties :: CLDeviceID -> IO [CLCommandQueueProperty]
clGetDeviceQueueProperties device = fmap (bitmaskToCommandQueueProperties . fromMaybe 0) $ getDeviceInfoUlong 0x102A device

-- | Describes single precision floating-point capability of the device. This is 
-- a bit-field that describes one or more of the 'CLDeviceFPConfig' values.
-- The mandated minimum floating-point capability is 'CL_FP_ROUND_TO_NEAREST' | 
-- 'CL_FP_INF_NAN'.
clGetDeviceSingleFPConfig :: CLDeviceID -> IO [CLDeviceFPConfig]
clGetDeviceSingleFPConfig = getDeviceInfoFP 0x101B

-- | The OpenCL device type. Currently supported values are one of or a 
-- combination of: 'CL_DEVICE_TYPE_CPU', 'CL_DEVICE_TYPE_GPU', 
-- 'CL_DEVICE_TYPE_ACCELERATOR', or 'CL_DEVICE_TYPE_DEFAULT'.
clGetDeviceType :: CLDeviceID -> IO [CLDeviceType]
clGetDeviceType device = fmap (bitmaskToDeviceTypes . fromMaybe 0) $ getDeviceInfoUlong 0x1000 device

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
