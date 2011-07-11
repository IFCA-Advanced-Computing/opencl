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
  CLPlatformInfo(..),
  -- * Functions
  clGetPlatformIDs, clGetPlatformInfo ) 
       where

-- -----------------------------------------------------------------------------
import Data.Maybe( fromMaybe )
import Foreign( Ptr, nullPtr, castPtr, alloca, allocaArray, peek, peekArray )
import Foreign.C.Types( CSize )
import Foreign.C.String( CString, peekCString )
import System.GPU.OpenCL.Types( CLPlatformID, CLuint, CLint )
import System.GPU.OpenCL.Errors( ErrorCode(..), clSuccess )

-- -----------------------------------------------------------------------------
foreign import ccall "clGetPlatformIDs" raw_clGetPlatformIDs :: 
  CLuint -> Ptr CLPlatformID -> Ptr CLuint -> IO CLint
foreign import ccall "clGetPlatformInfo" raw_clGetPlatformInfo :: 
  CLPlatformID -> CLuint -> CSize -> Ptr () -> Ptr CSize -> IO CLint 

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
