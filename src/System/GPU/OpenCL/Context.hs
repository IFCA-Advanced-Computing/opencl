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
module System.GPU.OpenCL.Context(
  -- * Types
  CLContext,
  -- * Context Functions
  clCreateContext, clCreateContextFromType, clRetainContext, clReleaseContext,
  clGetContextReferenceCount, clGetContextDevices )
    where

-- -----------------------------------------------------------------------------
import Foreign( 
  Ptr, FunPtr, nullPtr, castPtr, alloca, allocaArray, peek, peekArray, 
  pokeArray )
import Foreign.C.Types( CSize )
import Foreign.C.String( CString, peekCString )
import Foreign.Storable( sizeOf )
import System.GPU.OpenCL.Types( 
  CLuint, CLint, CLDeviceType_, CLContextInfo_, CLContextProperty_, ErrorCode(..),
  CLDeviceID, CLContext, CLDeviceType, bitmaskFromFlags, clSuccess )

-- -----------------------------------------------------------------------------
type ContextCallback = CString -> Ptr () -> CSize -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapContextCallback :: 
  ContextCallback -> IO (FunPtr ContextCallback)
foreign import ccall "clCreateContext" raw_clCreateContext ::
  Ptr CLContextProperty_ -> CLuint -> Ptr CLDeviceID -> FunPtr ContextCallback -> 
  Ptr () -> Ptr CLint -> IO CLContext
foreign import ccall "clCreateContextFromType" raw_clCreateContextFromType :: 
  Ptr CLContextProperty_ -> CLDeviceType_ -> FunPtr ContextCallback -> 
  Ptr () -> Ptr CLint -> IO CLContext
foreign import ccall "clRetainContext" raw_clRetainContext :: 
  CLContext -> IO CLint
foreign import ccall "clReleaseContext" raw_clReleaseContext :: 
  CLContext -> IO CLint
foreign import ccall "clGetContextInfo" raw_clGetContextInfo :: 
  CLContext -> CLContextInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

-- -----------------------------------------------------------------------------
mkContextCallback :: (String -> IO ()) -> ContextCallback
mkContextCallback f msg _ _ _ = peekCString msg >>= f

-- | Creates an OpenCL context.
-- An OpenCL context is created with one or more devices. Contexts are used by 
-- the OpenCL runtime for managing objects such as command-queues, memory, 
-- program and kernel objects and for executing kernels on one or more devices 
-- specified in the context.
clCreateContext :: [CLDeviceID] -> (String -> IO ()) -> IO (Maybe CLContext)
clCreateContext devs f = allocaArray ndevs $ \pdevs -> do
  pokeArray pdevs devs
  alloca $ \perr -> do
    fptr <- wrapContextCallback $ mkContextCallback f
    context <- raw_clCreateContext nullPtr cndevs pdevs fptr nullPtr perr
    errcode <- peek perr >>= return . ErrorCode
    if errcode == clSuccess
      then return . Just $ context
      else return Nothing
    where
      ndevs = length devs
      cndevs = fromIntegral ndevs

-- | Create an OpenCL context from a device type that identifies the specific 
-- device(s) to use.
clCreateContextFromType :: [CLDeviceType] -> (String -> IO ()) -> IO (Maybe CLContext)
clCreateContextFromType xs f = alloca $ \perr -> do
  fptr <- wrapContextCallback $ mkContextCallback f
  context <- raw_clCreateContextFromType nullPtr types fptr nullPtr perr
  errcode <- peek perr >>= return . ErrorCode
  if errcode == clSuccess
    then return . Just $ context
    else return Nothing
    where
      types = bitmaskFromFlags xs

-- | Increment the context reference count.
-- 'clCreateContext' and 'clCreateContextFromType' perform an implicit retain. 
-- This is very helpful for 3rd party libraries, which typically get a context 
-- passed to them by the application. However, it is possible that the 
-- application may delete the context without informing the library. Allowing 
-- functions to attach to (i.e. retain) and release a context solves the 
-- problem of a context being used by a library no longer being valid.
-- Returns 'True' if the function is executed successfully, or 'False' if 
-- context is not a valid OpenCL context.
clRetainContext :: CLContext -> IO Bool
clRetainContext ctx = raw_clRetainContext ctx 
                      >>= return . (==clSuccess) . ErrorCode

-- | Decrement the context reference count.
-- After the context reference count becomes zero and all the objects attached 
-- to context (such as memory objects, command-queues) are released, the 
-- context is deleted.
-- Returns 'True' if the function is executed successfully, or 'False' if 
-- context is not a valid OpenCL context.
clReleaseContext :: CLContext -> IO Bool
clReleaseContext ctx = raw_clReleaseContext ctx 
                       >>= return . (==clSuccess) . ErrorCode

getContextInfoSize :: CLContext -> CLContextInfo_ -> IO (Maybe CSize)
getContextInfoSize ctx infoid = alloca $ \(value_size :: Ptr CSize) -> do
  errcode <- fmap ErrorCode $ raw_clGetContextInfo ctx infoid 0 nullPtr value_size
  if errcode == clSuccess
    then fmap Just $ peek value_size
    else return Nothing

-- | Return the context reference count. The reference count returned should be 
-- considered immediately stale. It is unsuitable for general use in 
-- applications. This feature is provided for identifying memory leaks.
clGetContextReferenceCount :: CLContext -> IO (Maybe CLuint)
clGetContextReferenceCount ctx = alloca $ \(dat :: Ptr CLuint) -> do
  errcode <- fmap ErrorCode $ raw_clGetContextInfo ctx 0x1080 size (castPtr dat) nullPtr
  if errcode == clSuccess
    then fmap Just $ peek dat
    else return Nothing
    where 
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the list of devices in context.
clGetContextDevices :: CLContext -> IO [CLDeviceID]
clGetContextDevices ctx = do
  val <- getContextInfoSize ctx infoid
  case val of
    Nothing -> return []
    Just size -> let n = (fromIntegral size) `div` (sizeOf (nullPtr :: CLDeviceID))
                 in allocaArray n $ \(buff :: Ptr CLDeviceID) -> do
      errcode <- fmap ErrorCode $ raw_clGetContextInfo ctx infoid size (castPtr buff) nullPtr
      if errcode == clSuccess
        then peekArray n buff
        else return []
    where
      infoid = 0x1081

--data ContextProperty = CL_CONTEXT_PLATFORM CLPlatformID deriving( Show )

--contextPropertyToVal :: ContextProperty -> [CLContextProperty_]

-- | Return the properties argument specified in 'clCreateContext'.
--clGetContextProperties :: CLContext -> IO []
--clGetContextProperties ctx = do
--  val <- getContextInfoSize ctx infoid
--  case val of
--    Nothing -> return []
--    Just size
--        | size == 0 -> return []
--        | otherwise -> let n = (fromIntegral size) `div` (sizeOf (nullPtr :: Ptr (Ptr CInt)))
--                       in allocaArray n $ \(buff :: Ptr (Ptr CInt)) -> do
--      errcode <- fmap ErrorCode $ raw_clGetContextInfo ctx infoid size (castPtr buff) nullPtr
--      if errcode == clSuccess
--        then do
--          props <- peekArray n buff
--          print props
--          return []
--        else return []
--    where
--      infoid = 0x1082

-- -----------------------------------------------------------------------------
