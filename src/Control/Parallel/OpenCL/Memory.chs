{- Copyright (c) 2011 Luis Cabellos,

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of  nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, CPP #-}
module Control.Parallel.OpenCL.Memory(
  -- * Types
  CLMem, CLSampler, CLMemFlag(..), CLMemObjectType(..), CLAddressingMode(..), 
  CLFilterMode(..), CLImageFormat(..), CLChannelOrder(..), CLChannelType(..),
  -- * Memory Functions
  clCreateBuffer, clRetainMemObject, clReleaseMemObject, clGetMemType, 
  clGetMemFlags, clGetMemSize, clGetMemHostPtr, clGetMemMapCount, 
  clGetMemReferenceCount, clGetMemContext, clCreateFromGLBuffer,
  -- * Image Functions
  clCreateImage2D, clCreateImage3D, clCreateFromGLTexture2D,
  clGetSupportedImageFormats, clGetImageFormat, clGetImageElementSize, 
  clGetImageRowPitch, clGetImageSlicePitch, clGetImageWidth, clGetImageHeight, 
  clGetImageDepth,
  -- * Sampler Functions
  clCreateSampler, clRetainSampler, clReleaseSampler, clGetSamplerReferenceCount, 
  clGetSamplerContext, clGetSamplerAddressingMode, clGetSamplerFilterMode, 
  clGetSamplerNormalizedCoords
  ) where

-- -----------------------------------------------------------------------------
import Foreign
import Foreign.C.Types
import Control.Applicative( (<$>), (<*>) )
import Control.Parallel.OpenCL.Types( 
  CLMem, CLContext, CLSampler, CLint, CLuint, CLbool, CLMemFlags_,
  CLMemInfo_, CLAddressingMode_, CLFilterMode_, CLSamplerInfo_, CLImageInfo_,
  CLAddressingMode(..), CLFilterMode(..), CLMemFlag(..), CLMemObjectType_, 
  CLMemObjectType(..), 
  wrapPError, wrapCheckSuccess, wrapGetInfo, whenSuccess, getEnumCL, 
  bitmaskFromFlags, bitmaskToMemFlags, getCLValue )

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

-- -----------------------------------------------------------------------------
foreign import CALLCONV "clCreateBuffer" raw_clCreateBuffer :: 
  CLContext -> CLMemFlags_ -> CSize -> Ptr () -> Ptr CLint -> IO CLMem
foreign import CALLCONV "clCreateImage2D" raw_clCreateImage2D :: 
  CLContext -> CLMemFlags_ -> CLImageFormat_p -> CSize -> CSize -> CSize 
  -> Ptr () -> Ptr CLint -> IO CLMem
foreign import CALLCONV "clCreateImage3D" raw_clCreateImage3D :: 
  CLContext -> CLMemFlags_-> CLImageFormat_p -> CSize -> CSize -> CSize -> CSize 
  -> CSize -> Ptr () -> Ptr CLint -> IO CLMem
foreign import CALLCONV "clCreateFromGLTexture2D" raw_clCreateFromGLTexture2D ::
  CLContext -> CLMemFlags_ -> CLuint -> CLint -> CLuint -> Ptr CLint -> IO CLMem
foreign import CALLCONV "clCreateFromGLBuffer" raw_clCreateFromGLBuffer ::
  CLContext -> CLMemFlags_ -> CLuint -> Ptr CLint -> IO CLMem
foreign import CALLCONV "clRetainMemObject" raw_clRetainMemObject :: 
  CLMem -> IO CLint
foreign import CALLCONV "clReleaseMemObject" raw_clReleaseMemObject :: 
  CLMem -> IO CLint
foreign import CALLCONV "clGetSupportedImageFormats" raw_clGetSupportedImageFormats :: 
  CLContext -> CLMemFlags_ -> CLMemObjectType_ -> CLuint -> CLImageFormat_p 
  -> Ptr CLuint -> IO CLint
foreign import CALLCONV "clGetMemObjectInfo" raw_clGetMemObjectInfo :: 
  CLMem -> CLMemInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import CALLCONV "clGetImageInfo" raw_clGetImageInfo ::
  CLMem -> CLImageInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import CALLCONV "clCreateSampler" raw_clCreateSampler :: 
  CLContext -> CLbool -> CLAddressingMode_ -> CLFilterMode_ -> Ptr CLint -> IO CLSampler
foreign import CALLCONV "clRetainSampler" raw_clRetainSampler :: 
  CLSampler -> IO CLint
foreign import CALLCONV "clReleaseSampler" raw_clReleaseSampler :: 
  CLSampler -> IO CLint
foreign import CALLCONV "clGetSamplerInfo" raw_clGetSamplerInfo :: 
  CLSampler -> CLSamplerInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

-- -----------------------------------------------------------------------------
{-| Creates a buffer object. Returns a valid non-zero buffer object if the
buffer object is created successfully. Otherwise, it throws the 'CLError': 

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid.

 * 'CL_INVALID_BUFFER_SIZE' if size is 0 or is greater than
'clDeviceMaxMemAllocSize' value for all devices in context.

 * 'CL_INVALID_HOST_PTR' if host_ptr is NULL and 'CL_MEM_USE_HOST_PTR' or
'CL_MEM_COPY_HOST_PTR' are set in flags or if host_ptr is not NULL but
'CL_MEM_COPY_HOST_PTR' or 'CL_MEM_USE_HOST_PTR' are not set in flags.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for buffer object.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
clCreateBuffer :: Integral a => CLContext -> [CLMemFlag] -> (a, Ptr ()) -> IO CLMem
clCreateBuffer ctx xs (sbuff,buff) = wrapPError $ \perr -> do
  raw_clCreateBuffer ctx flags (fromIntegral sbuff) buff perr
    where
      flags = bitmaskFromFlags xs

{-| Creates an OpenCL buffer object from an OpenGL buffer object. Returns a valid non-zero OpenCL buffer object if the buffer object is created successfully. Otherwise it throws the 'CLError':
 * 'CL_INVALID_CONTEXT' if context is not a valid context or was not created from a GL context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid.

 * 'CL_INVALID_GL_OBJECT' if bufobj is not a GL buffer object or is a GL buffer object but does not have an existing data store.

 * 'CL_OUT_OF_RESOURCES' if there is a failure to allocate resources required by the OpenCL implementation on the device.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required by the OpenCL implementation on the host.
-}
clCreateFromGLBuffer :: Integral a => CLContext -> [CLMemFlag] -> a -> IO CLMem
clCreateFromGLBuffer ctx xs glObj = wrapPError $ \perr -> do
  raw_clCreateFromGLBuffer ctx flags cglObj perr
    where flags = bitmaskFromFlags xs
          cglObj = fromIntegral glObj
    
-- | Increments the memory object reference count. returns 'True' if the
-- function is executed successfully. After the memobj reference count becomes
-- zero and commands queued for execution on a command-queue(s) that use memobj
-- have finished, the memory object is deleted. It returns 'False' if memobj is
-- not a valid memory object.
clRetainMemObject :: CLMem -> IO Bool
clRetainMemObject mem = wrapCheckSuccess $ raw_clRetainMemObject mem

-- | Decrements the memory object reference count. After the memobj reference
-- count becomes zero and commands queued for execution on a command-queue(s)
-- that use memobj have finished, the memory object is deleted. Returns 'True'
-- if the function is executed successfully. It returns 'False' if memobj is not
-- a valid memory object.
clReleaseMemObject :: CLMem -> IO Bool
clReleaseMemObject mem = wrapCheckSuccess $ raw_clReleaseMemObject mem

-- -----------------------------------------------------------------------------
#c
enum CLChannelOrder {
  cL_R=CL_R,
  cL_A=CL_A,
  cL_INTENSITY=CL_INTENSITY,
  cL_LUMINANCE=CL_LUMINANCE,
  cL_RG=CL_RG,
  cL_RA=CL_RA,
  cL_RGB=CL_RGB,
  cL_RGBA=CL_RGBA,
  cL_ARGB=CL_ARGB,
  cL_BGRA=CL_BGRA,
  };
#endc
{-| Specifies the number of channels and the channel layout i.e. the memory
layout in which channels are stored in the image. Valid values are described in
the table below.
 
 * 'CL_R', 'CL_A'.

 * 'CL_INTENSITY', This format can only be used if channel data type =
'CL_UNORM_INT8', 'CL_UNORM_INT16', 'CL_SNORM_INT8', 'CL_SNORM_INT16',
'CL_HALF_FLOAT', or 'CL_FLOAT'.

 * 'CL_LUMINANCE', This format can only be used if channel data type =
'CL_UNORM_INT8', 'CL_UNORM_INT16', 'CL_SNORM_INT8', 'CL_SNORM_INT16',
'CL_HALF_FLOAT', or 'CL_FLOAT'.

 * 'CL_RG', 'CL_RA'.

 * 'CL_RGB', This format can only be used if channel data type =
'CL_UNORM_SHORT_565', 'CL_UNORM_SHORT_555' or 'CL_UNORM_INT101010'.

 * 'CL_RGBA'.

 * 'CL_ARGB', 'CL_BGRA'. This format can only be used if channel data type =
'CL_UNORM_INT8', 'CL_SNORM_INT8', 'CL_SIGNED_INT8' or 'CL_UNSIGNED_INT8'.  
-}
{#enum CLChannelOrder {upcaseFirstLetter} deriving(Show)#}

#c
enum CLChannelType {
  cL_SNORM_INT8=CL_SNORM_INT8,
  cL_SNORM_INT16=CL_SNORM_INT16,
  cL_UNORM_INT8=CL_UNORM_INT8,
  cL_UNORM_INT16=CL_UNORM_INT16,
  cL_UNORM_SHORT_565=CL_UNORM_SHORT_565,
  cL_UNORM_SHORT_555=CL_UNORM_SHORT_555,
  cL_UNORM_INT_101010=CL_UNORM_INT_101010,
  cL_SIGNED_INT8=CL_SIGNED_INT8,
  cL_SIGNED_INT16=CL_SIGNED_INT16,
  cL_SIGNED_INT32=CL_SIGNED_INT32,
  cL_UNSIGNED_INT8=CL_UNSIGNED_INT8,
  cL_UNSIGNED_INT16=CL_UNSIGNED_INT16,
  cL_UNSIGNED_INT32=CL_UNSIGNED_INT32,
  cL_HALF_FLOAT=CL_HALF_FLOAT,
  cL_FLOAT=CL_FLOAT,
  };
#endc
{-| Describes the size of the channel data type. The number of bits per element
determined by the image_channel_data_type and image_channel_order must be a
power of two. The list of supported values is described in the table below.

 * 'CL_SNORM_INT8', Each channel component is a normalized signed 8-bit integer
value.

 * 'CL_SNORM_INT16', Each channel component is a normalized signed 16-bit
integer value.

 * 'CL_UNORM_INT8', Each channel component is a normalized unsigned 8-bit
integer value.

 * 'CL_UNORM_INT16', Each channel component is a normalized unsigned 16-bit
integer value.

 * 'CL_UNORM_SHORT_565', Represents a normalized 5-6-5 3-channel RGB image. The
channel order must be 'CL_RGB'.

 * 'CL_UNORM_SHORT_555', Represents a normalized x-5-5-5 4-channel xRGB
image. The channel order must be 'CL_RGB'.

 * 'CL_UNORM_INT_101010', Represents a normalized x-10-10-10 4-channel xRGB
image. The channel order must be 'CL_RGB'.

 * 'CL_SIGNED_INT8', Each channel component is an unnormalized signed 8-bit
integer value.

 * 'CL_SIGNED_INT16', Each channel component is an unnormalized signed 16-bit
integer value.

 * 'CL_SIGNED_INT32', Each channel component is an unnormalized signed 32-bit
integer value.

 * 'CL_UNSIGNED_INT8', Each channel component is an unnormalized unsigned 8-bit
integer value.

 * 'CL_UNSIGNED_INT16', Each channel component is an unnormalized unsigned
16-bit integer value.

 * 'CL_UNSIGNED_INT32', Each channel component is an unnormalized unsigned
32-bit integer value.

 * 'CL_HALF_FLOAT', Each channel component is a 16-bit half-float value.

 * 'CL_FLOAT', Each channel component is a single precision floating-point
value.
-}
{#enum CLChannelType {upcaseFirstLetter} deriving(Show)#}

data CLImageFormat = CLImageFormat
                     { image_channel_order :: ! CLChannelOrder
                     , image_channel_data_type :: ! CLChannelType }
                     deriving( Show )
{#pointer *cl_image_format as CLImageFormat_p -> CLImageFormat#}
instance Storable CLImageFormat where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = {#sizeof cl_image_format #}
  peek p =
    CLImageFormat <$> fmap getEnumCL ({#get cl_image_format.image_channel_order #} p)
           <*> fmap getEnumCL ({#get cl_image_format.image_channel_data_type #} p)
  poke p (CLImageFormat a b) = do
    {#set cl_image_format.image_channel_order #} p (getCLValue a)
    {#set cl_image_format.image_channel_data_type #} p (getCLValue b)

-- -----------------------------------------------------------------------------
{-| Creates a 2D image object.

'clCreateImage2D' returns a valid non-zero image object created if the image
object is created successfully. Otherwise, it throws one of the following
'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid.

 * 'CL_INVALID_IMAGE_FORMAT_DESCRIPTOR' if values specified in image_format are
not valid.

 * 'CL_INVALID_IMAGE_SIZE' if image_width or image_height are 0 or if they
exceed values specified in 'CL_DEVICE_IMAGE2D_MAX_WIDTH' or
'CL_DEVICE_IMAGE2D_MAX_HEIGHT' respectively for all devices in context or if
values specified by image_row_pitch do not follow rules described in the
argument description above.

 * 'CL_INVALID_HOST_PTR' if host_ptr is 'nullPtr' and 'CL_MEM_USE_HOST_PTR' or
'CL_MEM_COPY_HOST_PTR' are set in flags or if host_ptr is not 'nullPtr' but
'CL_MEM_COPY_HOST_PTR' or 'CL_MEM_USE_HOST_PTR' are not set in flags.

 * 'CL_IMAGE_FORMAT_NOT_SUPPORTED' if the image_format is not supported.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for image object.

 * 'CL_INVALID_OPERATION' if there are no devices in context that support images
(i.e. 'CL_DEVICE_IMAGE_SUPPORT' (specified in the table of OpenCL Device Queries
for 'clGetDeviceInfo') is 'False').

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.

-}

clCreateImage2D :: Integral a => CLContext -- ^ A valid OpenCL context on which
                                           -- the image object is to be created.
                   -> [CLMemFlag] -- ^ A list of flags that is used to specify
                                  -- allocation and usage information about the
                                  -- image memory object being created.
                   -> CLImageFormat -- ^ Structure that describes format
                                    -- properties of the image to be allocated.
                   -> a -- ^ The width of the image in pixels. It must be values
                        -- greater than or equal to 1.
                   -> a -- ^ The height of the image in pixels. It must be
                        -- values greater than or equal to 1.
                   -> a -- ^ The scan-line pitch in bytes. This must be 0 if
                        -- host_ptr is 'nullPtr' and can be either 0 or greater
                        -- than or equal to image_width * size of element in
                        -- bytes if host_ptr is not 'nullPtr'. If host_ptr is
                        -- not 'nullPtr' and image_row_pitch is equal to 0,
                        -- image_row_pitch is calculated as image_width * size
                        -- of element in bytes. If image_row_pitch is not 0, it
                        -- must be a multiple of the image element size in
                        -- bytes.
                   -> Ptr () -- ^ A pointer to the image data that may already
                             -- be allocated by the application. The size of the
                             -- buffer that host_ptr points to must be greater
                             -- than or equal to image_row_pitch *
                             -- image_height. The size of each element in bytes
                             -- must be a power of 2. The image data specified
                             -- by host_ptr is stored as a linear sequence of
                             -- adjacent scanlines. Each scanline is stored as a
                             -- linear sequence of image elements.
                   -> IO CLMem
clCreateImage2D ctx xs fmt iw ih irp ptr = wrapPError $ \perr -> with fmt $ \pfmt -> do
  raw_clCreateImage2D ctx flags pfmt ciw cih cirp ptr perr
    where
      flags = bitmaskFromFlags xs
      ciw = fromIntegral iw
      cih = fromIntegral ih
      cirp = fromIntegral irp

{-| Creates a 3D image object.

'clCreateImage3D' returns a valid non-zero image object created if the image
object is created successfully. Otherwise, it throws one of the following
'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid.

 * 'CL_INVALID_IMAGE_FORMAT_DESCRIPTOR' if values specified in image_format are
not valid.

 * 'CL_INVALID_IMAGE_SIZE' if image_width, image_height are 0 or if image_depth
less than or equal to 1 or if they exceed values specified in
'CL_DEVICE_IMAGE3D_MAX_WIDTH', CL_DEVICE_IMAGE3D_MAX_HEIGHT' or
'CL_DEVICE_IMAGE3D_MAX_DEPTH' respectively for all devices in context or if
values specified by image_row_pitch and image_slice_pitch do not follow rules
described in the argument description above.

 * 'CL_INVALID_HOST_PTR' if host_ptr is 'nullPtr' and 'CL_MEM_USE_HOST_PTR' or
'CL_MEM_COPY_HOST_PTR' are set in flags or if host_ptr is not 'nullPtr' but
'CL_MEM_COPY_HOST_PTR' or 'CL_MEM_USE_HOST_PTR' are not set in flags.

 * 'CL_IMAGE_FORMAT_NOT_SUPPORTED' if the image_format is not supported.

 * 'CL_MEM_OBJECT_ALLOCATION_FAILURE' if there is a failure to allocate memory
for image object.

 * 'CL_INVALID_OPERATION' if there are no devices in context that support images
(i.e. 'CL_DEVICE_IMAGE_SUPPORT' (specified in the table of OpenCL Device Queries
for clGetDeviceInfo) is 'False').

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.

-}
clCreateImage3D :: Integral a => CLContext -- ^ A valid OpenCL context on which
                                           -- the image object is to be created.
                   -> [CLMemFlag] -- ^ A list of flags that is used to specify
                                  -- allocation and usage information about the
                                  -- image memory object being created.
                   -> CLImageFormat -- ^ Structure that describes format
                                    -- properties of the image to be allocated.
                   -> a -- ^ The width of the image in pixels. It must be values
                        -- greater than or equal to 1.
                   -> a -- ^ The height of the image in pixels. It must be
                        -- values greater than or equal to 1.
                   -> a -- ^ The depth of the image in pixels. This must be a
                        -- value greater than 1.
                   -> a -- ^ The scan-line pitch in bytes. This must be 0 if
                        -- host_ptr is 'nullPtr' and can be either 0 or greater
                        -- than or equal to image_width * size of element in
                        -- bytes if host_ptr is not 'nullPtr'. If host_ptr is
                        -- not 'nullPtr' and image_row_pitch is equal to 0,
                        -- image_row_pitch is calculated as image_width * size
                        -- of element in bytes. If image_row_pitch is not 0, it
                        -- must be a multiple of the image element size in
                        -- bytes.
                   -> a -- ^ The size in bytes of each 2D slice in the 3D
                        -- image. This must be 0 if host_ptr is 'nullPtr' and
                        -- can be either 0 or greater than or equal to
                        -- image_row_pitch * image_height if host_ptr is not
                        -- 'nullPtr'. If host_ptr is not 'nullPtr' and
                        -- image_slice_pitch equal to 0, image_slice_pitch is
                        -- calculated as image_row_pitch * image_height. If
                        -- image_slice_pitch is not 0, it must be a multiple of
                        -- the image_row_pitch.
                   -> Ptr () -- ^ A pointer to the image data that may already
                             -- be allocated by the application. The size of the
                             -- buffer that host_ptr points to must be greater
                             -- than or equal to image_slice_pitch *
                             -- image_depth. The size of each element in bytes
                             -- must be a power of 2. The image data specified
                             -- by host_ptr is stored as a linear sequence of
                             -- adjacent 2D slices. Each 2D slice is a linear
                             -- sequence of adjacent scanlines. Each scanline is
                             -- a linear sequence of image elements.
                   -> IO CLMem
clCreateImage3D ctx xs fmt iw ih idepth irp isp ptr = wrapPError $ \perr -> with fmt $ \pfmt -> do
  raw_clCreateImage3D ctx flags pfmt ciw cih cid cirp cisp ptr perr
    where
      flags = bitmaskFromFlags xs
      ciw = fromIntegral iw
      cih = fromIntegral ih
      cid = fromIntegral idepth
      cirp = fromIntegral irp
      cisp = fromIntegral isp  

{-| Creates a 2D OpenCL image object from an existing OpenGL texture.

'clCreateFromGLTexture2D' returns a non-zero image object if the image
object is created successfully. Otherwise, it throws one of the
following 'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context or was not
created from a GL context.

 * 'CL_INVALID_VALUE' if values specified in flags are not valid or if
value specified in texture_target is not one of the values specified
in the description of texture_target.

 * 'CL_INVALID_MIPLEVEL' if miplevel is less than the value of
levelbase (for OpenGL implementations) or zero (for OpenGL ES
implementations); or greater than the value of q (for both OpenGL and
OpenGL ES). levelbase and q are defined for the texture in section
3.8.10 (Texture Completeness) of the OpenGL 2.1 specification and
section 3.7.10 of the OpenGL ES 2.0 specification.

 * 'CL_INVALID_MIPLEVEL' if miplevel is greater than zero and the
OpenGL implementation does not support creating from non-zero mipmap
levels.

 * 'CL_INVALID_GL_OBJECT' if texture is not a GL texture object whose
type matches texture_target, if the specified miplevel of texture is
not defined, or if the width or height of the specified miplevel is
zero.

 * 'CL_INVALID_IMAGE_FORMAT_DESCRIPTOR' if the OpenGL texture internal
format does not map to a supported OpenCL image format.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources
required by the OpenCL implementation on the host.

-}
clCreateFromGLTexture2D :: (Integral a, Integral b, Integral c) =>
                           CLContext -- ^ A valid OpenCL context in
                                     -- which the image object is to
                                     -- be created.
                        -> [CLMemFlag] -- ^ A list of flags that is
                                       -- used to specify usage
                                       -- information about the image
                                       -- memory object being created.
                        -> a -- ^ The OpenGL image type of the texture
                             -- (e.g. GL_TEXTURE_2D)
                        -> b -- ^ The mipmap level to be used.
                        -> c -- ^ The GL texture object name.
                        -> IO CLMem
clCreateFromGLTexture2D ctx xs texType mipLevel tex = 
  wrapPError $ raw_clCreateFromGLTexture2D ctx flags cTexType cMip cTex
    where flags = bitmaskFromFlags xs
          cTexType = fromIntegral texType
          cMip = fromIntegral mipLevel
          cTex = fromIntegral tex
      
getNumSupportedImageFormats :: CLContext -> [CLMemFlag] -> CLMemObjectType -> IO CLuint
getNumSupportedImageFormats ctx xs mtype = alloca $ \(value_size :: Ptr CLuint) -> do
  whenSuccess (raw_clGetSupportedImageFormats ctx flags (getCLValue mtype) 0 nullPtr value_size)
    $ peek value_size
    where
      flags = bitmaskFromFlags xs
  
{-| Get the list of image formats supported by an OpenCL
implementation. 'clGetSupportedImageFormats' can be used to get the list of
image formats supported by an OpenCL implementation when the following
information about an image memory object is specified:

 * Context
 * Image type - 2D or 3D image
 * Image object allocation information

Throws 'CL_INVALID_CONTEXT' if context is not a valid context, throws
'CL_INVALID_VALUE' if flags or image_type are not valid.

-}
clGetSupportedImageFormats :: CLContext -- ^ A valid OpenCL context on which the
                                        -- image object(s) will be created.
                              -> [CLMemFlag] -- ^ A bit-field that is used to
                                             -- specify allocation and usage
                                             -- information about the image
                                             -- memory object.
                              -> CLMemObjectType -- ^ Describes the image type
                                                 -- and must be either
                                                 -- 'CL_MEM_OBJECT_IMAGE2D' or
                                                 -- 'CL_MEM_OBJECT_IMAGE3D'.
                              -> IO [CLImageFormat]
clGetSupportedImageFormats ctx xs mtype = do
  num <- getNumSupportedImageFormats ctx xs mtype
  allocaArray (fromIntegral num) $ \(buff :: Ptr CLImageFormat) -> do
    whenSuccess (raw_clGetSupportedImageFormats ctx flags (getCLValue mtype) num (castPtr buff) nullPtr)
      $ peekArray (fromIntegral num) buff
    where
      flags = bitmaskFromFlags xs

-- -----------------------------------------------------------------------------
#c
enum CLImageInfo {
  cL_IMAGE_FORMAT=CL_IMAGE_FORMAT,
  cL_IMAGE_ELEMENT_SIZE=CL_IMAGE_ELEMENT_SIZE,
  cL_IMAGE_ROW_PITCH=CL_IMAGE_ROW_PITCH,
  cL_IMAGE_SLICE_PITCH=CL_IMAGE_SLICE_PITCH,
  cL_IMAGE_WIDTH=CL_IMAGE_WIDTH,
  cL_IMAGE_HEIGHT=CL_IMAGE_HEIGHT,
  cL_IMAGE_DEPTH=CL_IMAGE_DEPTH,
  };
#endc
{#enum CLImageInfo {upcaseFirstLetter} #}

-- | Return image format descriptor specified when image is created with
-- clCreateImage2D or clCreateImage3D.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_FORMAT'.
clGetImageFormat :: CLMem -> IO CLImageFormat
clGetImageFormat mem =
  wrapGetInfo (\(dat :: Ptr CLImageFormat) ->
                raw_clGetImageInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_FORMAT
      size = fromIntegral $ sizeOf (undefined :: CLImageFormat)
  
-- | Return size of each element of the image memory object given by image. An
-- element is made up of n channels. The value of n is given in 'CLImageFormat'
-- descriptor.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_ELEMENT_SIZE'.
clGetImageElementSize :: CLMem -> IO CSize      
clGetImageElementSize mem =
  wrapGetInfo (\(dat :: Ptr CSize) ->
                raw_clGetImageInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_ELEMENT_SIZE
      size = fromIntegral $ sizeOf (undefined :: CSize)
      
-- | Return size in bytes of a row of elements of the image object given by
-- image.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_ROW_PITCH'.
clGetImageRowPitch :: CLMem -> IO CSize      
clGetImageRowPitch mem = 
  wrapGetInfo (\(dat :: Ptr CSize) ->
                raw_clGetImageInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_ROW_PITCH
      size = fromIntegral $ sizeOf (undefined :: CSize)
      
-- | Return size in bytes of a 2D slice for the 3D image object given by
-- image. For a 2D image object this value will be 0.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_SLICE_PITCH'.
clGetImageSlicePitch :: CLMem -> IO CSize      
clGetImageSlicePitch mem = 
  wrapGetInfo (\(dat :: Ptr CSize) ->
                raw_clGetImageInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_SLICE_PITCH
      size = fromIntegral $ sizeOf (undefined :: CSize)      
      
-- | Return width of image in pixels.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_WIDTH'.
clGetImageWidth :: CLMem -> IO CSize      
clGetImageWidth mem = 
  wrapGetInfo (\(dat :: Ptr CSize) ->
                raw_clGetImageInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_WIDTH
      size = fromIntegral $ sizeOf (undefined :: CSize)
      
-- | Return height of image in pixels.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_HEIGHT'.
clGetImageHeight :: CLMem -> IO CSize      
clGetImageHeight mem = 
  wrapGetInfo (\(dat :: Ptr CSize) ->
                raw_clGetImageInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_HEIGHT
      size = fromIntegral $ sizeOf (undefined :: CSize)

-- | Return depth of the image in pixels. For a 2D image, depth equals 0.
--
-- This function execute OpenCL clGetImageInfo with 'CL_IMAGE_DEPTH'.
clGetImageDepth :: CLMem -> IO CSize      
clGetImageDepth mem = 
  wrapGetInfo (\(dat :: Ptr CSize) ->
                raw_clGetImageInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_IMAGE_DEPTH
      size = fromIntegral $ sizeOf (undefined :: CSize)

-- -----------------------------------------------------------------------------
#c
enum CLMemInfo {
  cL_MEM_TYPE=CL_MEM_TYPE,
  cL_MEM_FLAGS=CL_MEM_FLAGS,
  cL_MEM_SIZE=CL_MEM_SIZE,
  cL_MEM_HOST_PTR=CL_MEM_HOST_PTR,
  cL_MEM_MAP_COUNT=CL_MEM_MAP_COUNT,
  cL_MEM_REFERENCE_COUNT=CL_MEM_REFERENCE_COUNT,
  cL_MEM_CONTEXT=CL_MEM_CONTEXT,
  };
#endc
{#enum CLMemInfo {upcaseFirstLetter} #}

-- | Returns the mem object type.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_TYPE'.
clGetMemType :: CLMem -> IO CLMemObjectType
clGetMemType mem =
    wrapGetInfo (\(dat :: Ptr CLMemObjectType_) ->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_MEM_TYPE
      size = fromIntegral $ sizeOf (0::CLMemObjectType_)

-- | Return the flags argument value specified when memobj was created.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_FLAGS'.
clGetMemFlags :: CLMem -> IO [CLMemFlag]
clGetMemFlags mem =
    wrapGetInfo (\(dat :: Ptr CLMemFlags_)->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) bitmaskToMemFlags
    where 
      infoid = getCLValue CL_MEM_FLAGS
      size = fromIntegral $ sizeOf (0::CLMemFlags_)

-- | Return actual size of memobj in bytes.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_SIZE'.
clGetMemSize :: CLMem -> IO CSize
clGetMemSize mem =
    wrapGetInfo (\(dat :: Ptr CSize)->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_SIZE
      size = fromIntegral $ sizeOf (0::CSize)

-- | Return the host_ptr argument value specified when memobj is created.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_HOST_PTR'.
clGetMemHostPtr :: CLMem -> IO (Ptr ())
clGetMemHostPtr mem =
    wrapGetInfo (\(dat :: Ptr (Ptr ()))->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_HOST_PTR
      size = fromIntegral $ sizeOf (nullPtr::Ptr ())

-- | Map count. The map count returned should be considered immediately
-- stale. It is unsuitable for general use in applications. This feature is
-- provided for debugging.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_MAP_COUNT'.
clGetMemMapCount :: CLMem -> IO CLuint
clGetMemMapCount mem =
    wrapGetInfo (\(dat :: Ptr CLuint)->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_MAP_COUNT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- | Return memobj reference count. The reference count returned should be
-- considered immediately stale. It is unsuitable for general use in
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_REFERENCE_COUNT'.
clGetMemReferenceCount :: CLMem -> IO CLuint
clGetMemReferenceCount mem =
    wrapGetInfo (\(dat :: Ptr CLuint)->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- | Return context specified when memory object is created.
--
-- This function execute OpenCL clGetMemObjectInfo with 'CL_MEM_CONTEXT'.
clGetMemContext :: CLMem -> IO CLContext
clGetMemContext mem =
    wrapGetInfo (\(dat :: Ptr CLContext)->
        raw_clGetMemObjectInfo mem infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_MEM_CONTEXT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- -----------------------------------------------------------------------------
{-| Creates a sampler object. A sampler object describes how to sample an image
when the image is read in the kernel. The built-in functions to read from an
image in a kernel take a sampler as an argument. The sampler arguments to the
image read function can be sampler objects created using OpenCL functions and
passed as argument values to the kernel or can be samplers declared inside a
kernel. In this section we discuss how sampler objects are created using OpenCL
functions.

Returns a valid non-zero sampler object if the sampler object is created
successfully. Otherwise, it throws one of the following 'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_INVALID_VALUE' if addressing_mode, filter_mode, or normalized_coords or a
combination of these argument values are not valid.

 * 'CL_INVALID_OPERATION' if images are not supported by any device associated
with context (i.e. 'CL_DEVICE_IMAGE_SUPPORT' specified in the table of OpenCL
Device Queries for clGetDeviceInfo is 'False').

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.
-}
clCreateSampler :: CLContext -> Bool -> CLAddressingMode -> CLFilterMode 
                   -> IO CLSampler
clCreateSampler ctx norm am fm = wrapPError $ \perr -> do
  raw_clCreateSampler ctx (fromBool norm) (getCLValue am) (getCLValue fm) perr

-- | Increments the sampler reference count. 'clCreateSampler' does an implicit
-- retain. Returns 'True' if the function is executed successfully. It returns
-- 'False' if sampler is not a valid sampler object.
clRetainSampler :: CLSampler -> IO Bool
clRetainSampler mem = wrapCheckSuccess $ raw_clRetainSampler mem

-- | Decrements the sampler reference count. The sampler object is deleted after
-- the reference count becomes zero and commands queued for execution on a
-- command-queue(s) that use sampler have finished. 'clReleaseSampler' returns
-- 'True' if the function is executed successfully. It returns 'False' if
-- sampler is not a valid sampler object.
clReleaseSampler :: CLSampler -> IO Bool
clReleaseSampler mem = wrapCheckSuccess $ raw_clReleaseSampler mem

#c
enum CLSamplerInfo {
  cL_SAMPLER_REFERENCE_COUNT=CL_SAMPLER_REFERENCE_COUNT,
  cL_SAMPLER_CONTEXT=CL_SAMPLER_CONTEXT,
  cL_SAMPLER_ADDRESSING_MODE=CL_SAMPLER_ADDRESSING_MODE,
  cL_SAMPLER_FILTER_MODE=CL_SAMPLER_FILTER_MODE,
  cL_SAMPLER_NORMALIZED_COORDS=CL_SAMPLER_NORMALIZED_COORDS
  };
#endc
{#enum CLSamplerInfo {upcaseFirstLetter} #}

-- | Return the sampler reference count. The reference count returned should be
-- considered immediately stale. It is unsuitable for general use in
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetSamplerInfo with
-- 'CL_SAMPLER_REFERENCE_COUNT'.
clGetSamplerReferenceCount :: CLSampler -> IO CLuint
clGetSamplerReferenceCount sam =
    wrapGetInfo (\(dat :: Ptr CLuint)->
        raw_clGetSamplerInfo sam infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_SAMPLER_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0 :: CLuint)

-- | Return the context specified when the sampler is created.
--
-- This function execute OpenCL clGetSamplerInfo with 'CL_SAMPLER_CONTEXT'.
clGetSamplerContext :: CLSampler -> IO CLContext
clGetSamplerContext sam =
    wrapGetInfo (\(dat :: Ptr CLContext)->
        raw_clGetSamplerInfo sam infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_SAMPLER_CONTEXT
      size = fromIntegral $ sizeOf (nullPtr :: CLContext)

-- | Return the value specified by addressing_mode argument to clCreateSampler.
--
-- This function execute OpenCL clGetSamplerInfo with
-- 'CL_SAMPLER_ADDRESSING_MODE'.
clGetSamplerAddressingMode :: CLSampler -> IO CLAddressingMode
clGetSamplerAddressingMode sam =
    wrapGetInfo (\(dat :: Ptr CLAddressingMode_)->
        raw_clGetSamplerInfo sam infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_SAMPLER_ADDRESSING_MODE
      size = fromIntegral $ sizeOf (0 :: CLAddressingMode_)

-- | Return the value specified by filter_mode argument to clCreateSampler.
--
-- This function execute OpenCL clGetSamplerInfo with 'CL_SAMPLER_FILTER_MODE'.
clGetSamplerFilterMode :: CLSampler -> IO CLFilterMode
clGetSamplerFilterMode sam =
    wrapGetInfo (\(dat :: Ptr CLFilterMode_)->
        raw_clGetSamplerInfo sam infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_SAMPLER_FILTER_MODE
      size = fromIntegral $ sizeOf (0 :: CLFilterMode_)

-- | Return the value specified by normalized_coords argument to
-- clCreateSampler.
--
-- This function execute OpenCL clGetSamplerInfo with
-- 'CL_SAMPLER_NORMALIZED_COORDS'.
clGetSamplerNormalizedCoords :: CLSampler -> IO Bool
clGetSamplerNormalizedCoords sam =
    wrapGetInfo (\(dat :: Ptr CLbool)->
        raw_clGetSamplerInfo sam infoid size (castPtr dat)) (/=0)
    where 
      infoid = getCLValue CL_SAMPLER_NORMALIZED_COORDS
      size = fromIntegral $ sizeOf (0 :: CLbool)

-- -----------------------------------------------------------------------------
