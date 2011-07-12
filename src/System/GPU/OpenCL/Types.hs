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
  ErrorCode(..), CLPlatformID, CLDeviceID, CLContext, 
  CLDeviceType(..), getDeviceTypeValue, bitmaskToDeviceTypes, 
  bitmaskFromDeviceTypes ) 
       where

-- -----------------------------------------------------------------------------
import Foreign( Ptr )
import Foreign.C.Types( CInt, CULong )
import Data.Maybe( fromMaybe, mapMaybe )
import Data.List( foldl1' )
import Data.Bits( shiftL, complement, (.|.) )
import System.GPU.OpenCL.Util( testMask )

-- -----------------------------------------------------------------------------

data PlatformIDc = PlatformIDc
data DeviceIDc = DeviceIDc
data Contextc = Contextc

type CLPlatformID = Ptr PlatformIDc
type CLDeviceID = Ptr DeviceIDc
type CLContext = Ptr Contextc

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

deviceTypeValues :: [(CLDeviceType,CULong)]
deviceTypeValues = [ 
  (CL_DEVICE_TYPE_CPU, 1 `shiftL` 1), (CL_DEVICE_TYPE_GPU, 1 `shiftL` 2), 
  (CL_DEVICE_TYPE_ACCELERATOR, 1 `shiftL` 3), (CL_DEVICE_TYPE_DEFAULT, 1 `shiftL` 0),
  (CL_DEVICE_TYPE_ALL, complement 0) ]
getDeviceTypeValue :: CLDeviceType -> CULong
getDeviceTypeValue info = fromMaybe 0 (lookup info deviceTypeValues)

bitmaskToDeviceTypes :: CULong -> [CLDeviceType]
bitmaskToDeviceTypes mask = map fst . filter (testMask mask) $ deviceTypeValues

bitmaskFromDeviceTypes :: [CLDeviceType] -> CULong
bitmaskFromDeviceTypes = foldl1' (.|.) . mapMaybe (`lookup` deviceTypeValues)
        
-- -----------------------------------------------------------------------------
