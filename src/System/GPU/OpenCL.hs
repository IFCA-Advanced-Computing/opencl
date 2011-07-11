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
module System.GPU.OpenCL( clGetPlatformIDs ) where

import Foreign( Ptr, nullPtr, alloca, allocaArray, peek, peekArray )
import System.GPU.OpenCL.Types( CLPlatformID, CLuint, CLint )
import System.GPU.OpenCL.Errors( ErrorCode(..), clSuccess )

foreign import ccall "clGetPlatformIDs" raw_clGetPlatformIDs :: 
  CLuint -> Ptr CLPlatformID -> Ptr CLuint -> IO CLint

getNumPlatforms :: IO (Maybe CLuint)
getNumPlatforms = alloca $ \(num_platforms :: Ptr CLuint) -> do
  errcode <- fmap ErrorCode $ raw_clGetPlatformIDs 0 nullPtr num_platforms
  if errcode == clSuccess
    then fmap Just $ peek num_platforms
    else return Nothing

clGetPlatformIDs :: IO [CLPlatformID]
clGetPlatformIDs = do
  nplats <- getNumPlatforms
  case nplats of
    Nothing -> return []
    Just n -> allocaArray (fromIntegral n) $ \(platforms :: Ptr CLPlatformID) -> do
      errcode <- fmap ErrorCode $ raw_clGetPlatformIDs n platforms nullPtr
      if errcode == clSuccess
        then peekArray (fromIntegral n) platforms
        else return []
