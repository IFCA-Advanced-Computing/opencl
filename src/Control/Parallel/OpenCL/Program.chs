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
module Control.Parallel.OpenCL.Program(  
  -- * Types
  CLProgram, CLBuildStatus(..), CLKernel,
  -- * Program Functions
  clCreateProgramWithSource, clCreateProgramWithBinary, clRetainProgram, 
  clReleaseProgram, clUnloadCompiler, clBuildProgram, 
  clGetProgramReferenceCount, clGetProgramContext, clGetProgramNumDevices, 
  clGetProgramDevices, clGetProgramSource, clGetProgramBinarySizes, 
  clGetProgramBinaries, clGetProgramBuildStatus, clGetProgramBuildOptions, 
  clGetProgramBuildLog,
  -- * Kernel Functions
  clCreateKernel, clCreateKernelsInProgram, clRetainKernel, clReleaseKernel, 
  clSetKernelArg, clSetKernelArgSto, clGetKernelFunctionName, clGetKernelNumArgs, 
  clGetKernelReferenceCount, clGetKernelContext, clGetKernelProgram, 
  clGetKernelWorkGroupSize, clGetKernelCompileWorkGroupSize, 
  clGetKernelLocalMemSize
  ) where

-- -----------------------------------------------------------------------------
import Control.Monad( zipWithM, forM )
import Foreign
import Foreign.C.Types
import Foreign.C.String( CString, withCString, peekCString )
import Control.Parallel.OpenCL.Types( 
  CLint, CLuint, CLulong, CLProgram, CLContext, CLKernel, CLDeviceID, CLError,
  CLProgramInfo_, CLBuildStatus(..), CLBuildStatus_, CLProgramBuildInfo_, 
  CLKernelInfo_, CLKernelWorkGroupInfo_, wrapCheckSuccess, 
  whenSuccess, wrapPError, wrapGetInfo, getCLValue, getEnumCL )

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

-- -----------------------------------------------------------------------------
type BuildCallback = CLProgram -> Ptr () -> IO ()
foreign import CALLCONV "clCreateProgramWithSource" raw_clCreateProgramWithSource :: 
  CLContext -> CLuint -> Ptr CString -> Ptr CSize -> Ptr CLint -> IO CLProgram
foreign import CALLCONV "clCreateProgramWithBinary" raw_clCreateProgramWithBinary :: 
  CLContext -> CLuint -> Ptr CLDeviceID -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CLint -> Ptr CLint -> IO CLProgram
foreign import CALLCONV "clRetainProgram" raw_clRetainProgram :: 
  CLProgram -> IO CLint
foreign import CALLCONV "clReleaseProgram" raw_clReleaseProgram :: 
  CLProgram -> IO CLint
foreign import CALLCONV "clBuildProgram" raw_clBuildProgram :: 
  CLProgram -> CLuint -> Ptr CLDeviceID -> CString -> FunPtr BuildCallback -> Ptr () -> IO CLint
foreign import CALLCONV "clUnloadCompiler" raw_clUnloadCompiler :: 
  IO CLint
foreign import CALLCONV "clGetProgramInfo" raw_clGetProgramInfo :: 
  CLProgram -> CLProgramInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import CALLCONV "clGetProgramBuildInfo"  raw_clGetProgramBuildInfo :: 
  CLProgram -> CLDeviceID -> CLProgramBuildInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import CALLCONV "clCreateKernel" raw_clCreateKernel :: 
  CLProgram -> CString -> Ptr CLint -> IO CLKernel 
foreign import CALLCONV "clCreateKernelsInProgram" raw_clCreateKernelsInProgram :: 
  CLProgram -> CLuint -> Ptr CLKernel -> Ptr CLuint -> IO CLint 
foreign import CALLCONV "clRetainKernel" raw_clRetainKernel :: 
  CLKernel -> IO CLint 
foreign import CALLCONV "clReleaseKernel" raw_clReleaseKernel :: 
  CLKernel -> IO CLint 
foreign import CALLCONV "clSetKernelArg" raw_clSetKernelArg :: 
  CLKernel -> CLuint -> CSize -> Ptr () -> IO CLint
foreign import CALLCONV "clGetKernelInfo" raw_clGetKernelInfo :: 
  CLKernel -> CLKernelInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import CALLCONV "clGetKernelWorkGroupInfo" raw_clGetKernelWorkGroupInfo :: 
  CLKernel -> CLDeviceID -> CLKernelWorkGroupInfo_ -> CSize -> Ptr () -> Ptr CSize -> IO CLint

-- -----------------------------------------------------------------------------
{-| Creates a program object for a context, and loads the source code specified
by the text strings in the strings array into the program object. The devices
associated with the program object are the devices associated with context.

OpenCL allows applications to create a program object using the program source
or binary and build appropriate program executables. This allows applications to
determine whether they want to use the pre-built offline binary or load and
compile the program source and use the executable compiled/linked online as the
program executable. This can be very useful as it allows applications to load
and build program executables online on its first instance for appropriate
OpenCL devices in the system. These executables can now be queried and cached by
the application. Future instances of the application launching will no longer
need to compile and build the program executables. The cached executables can be
read and loaded by the application, which can help significantly reduce the
application initialization time.

An OpenCL program consists of a set of kernels that are identified as functions
declared with the __kernel qualifier in the program source. OpenCL programs may
also contain auxiliary functions and constant data that can be used by __kernel
functions. The program executable can be generated online or offline by the
OpenCL compiler for the appropriate target device(s).

'clCreateProgramWithSource' returns a valid non-zero program object if the
program object is created successfully. Otherwise, it throws one of the
following 'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.  
-}
clCreateProgramWithSource :: CLContext -> String -> IO CLProgram
clCreateProgramWithSource ctx source =
  withCString source $ \cSource ->
  withArray [cSource] $ \sourcesP ->
  wrapPError (raw_clCreateProgramWithSource ctx 1 sourcesP nullPtr)

{-| Creates a program object for a context, and loads specified binary data into
the program object.

The program binaries specified by binaries contain the bits that describe the
program executable that will be run on the device(s) associated with
context. The program binary can consist of either or both of device-specific
executable(s), and/or implementation-specific intermediate representation (IR)
which will be converted to the device-specific executable.

OpenCL allows applications to create a program object using the program
source or binary and build appropriate program executables. This allows
applications to determine whether they want to use the pre-built offline binary
or load and compile the program source and use the executable compiled/linked
online as the program executable. This can be very useful as it allows
applications to load and build program executables online on its first instance
for appropriate OpenCL devices in the system. These executables can now be
queried and cached by the application. Future instances of the application
launching will no longer need to compile and build the program executables. The
cached executables can be read and loaded by the application, which can help
significantly reduce the application initialization time.

Returns a valid non-zero program object and a list of 'CLError' values whether
the program binary for each device specified in device_list was loaded
successfully or not. It is list of the same length the list of devices with
'CL_SUCCESS' if binary was successfully loaded for device specified by same
position; otherwise returns 'CL_INVALID_VALUE' if length of binary is zero or
'CL_INVALID_BINARY' if program binary is not a valid binary
for the specified device.

The function can throw on of the following 'CLError' exceptions:

 * 'CL_INVALID_CONTEXT' if context is not a valid context.  

 * 'CL_INVALID_VALUE' if the device list is empty; or if lengths or binaries are
empty.

 * 'CL_INVALID_DEVICE' if OpenCL devices listed in the device list are not in
the list of devices associated with context.

 * 'CL_INVALID_BINARY' if an invalid program binary was encountered for any
device.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.  
-} 
clCreateProgramWithBinary :: CLContext -> [CLDeviceID] -> [[Word8]] 
                             -> IO (CLProgram, [CLError])
clCreateProgramWithBinary ctx devs bins = wrapPError $ \perr ->
  withArray devs $ \pdevs ->
    withArray lbins $ \plbins -> do
      buffs <- forM bins $ \bs -> do
        buff <- mallocArray (length bs) :: IO (Ptr Word8)
        pokeArray buff bs
        return buff

      ret <- withArray buffs $ \(pbuffs :: Ptr (Ptr Word8)) -> do
        allocaArray ndevs $ \(perrs :: Ptr CLint) -> do
          prog <- raw_clCreateProgramWithBinary ctx (fromIntegral ndevs) pdevs plbins pbuffs perrs perr
          errs <- peekArray ndevs perrs
          return (prog, map getEnumCL errs)

      mapM_ free buffs
      return ret
    
    where
      lbins = map (fromIntegral . length) bins :: [CSize]
      ndevs = length devs

-- | Increments the program reference count. 'clRetainProgram' returns 'True' if 
-- the function is executed successfully. It returns 'False' if program is not a 
-- valid program object.
clRetainProgram :: CLProgram -> IO Bool
clRetainProgram prg = wrapCheckSuccess $ raw_clRetainProgram prg

-- | Decrements the program reference count. The program object is deleted after 
-- all kernel objects associated with program have been deleted and the program 
-- reference count becomes zero. 'clReleseProgram' returns 'True' if 
-- the function is executed successfully. It returns 'False' if program is not a 
-- valid program object.
clReleaseProgram :: CLProgram -> IO Bool
clReleaseProgram prg = wrapCheckSuccess $ raw_clReleaseProgram prg

-- | Allows the implementation to release the resources allocated by the OpenCL
-- compiler. This is a hint from the application and does not guarantee that the
-- compiler will not be used in the future or that the compiler will actually be
-- unloaded by the implementation. Calls to 'clBuildProgram' after
-- 'clUnloadCompiler' will reload the compiler, if necessary, to build the
-- appropriate program executable.
clUnloadCompiler :: IO ()
clUnloadCompiler = raw_clUnloadCompiler >> return ()

{-| Builds (compiles and links) a program executable from the program source or
binary. OpenCL allows program executables to be built using the source or the
binary. The build options are categorized as pre-processor options, options for
math intrinsics, options that control optimization and miscellaneous
options. This specification defines a standard set of options that must be
supported by an OpenCL compiler when building program executables online or
offline. These may be extended by a set of vendor- or platform-specific options.

 * Preprocessor Options

These options control the OpenCL preprocessor which is run on each program
source before actual compilation. -D options are processed in the order they are
given in the options argument to clBuildProgram.

 [-D name] Predefine name as a macro, with definition 1.

 [-D name=definition] The contents of definition are tokenized and processed as
if they appeared during translation phase three in a `#define' directive. In
particular, the definition will be truncated by embedded newline characters.

 [-I dir] Add the directory dir to the list of directories to be searched for
header files.

 * Math Intrinsics Options

These options control compiler behavior regarding floating-point
arithmetic. These options trade off between speed and correctness.

 [-cl-single-precision-constant] Treat double precision floating-point constant
as single precision constant.

 [-cl-denorms-are-zero] This option controls how single precision and double
precision denormalized numbers are handled. If specified as a build option, the
single precision denormalized numbers may be flushed to zero and if the optional
extension for double precision is supported, double precision denormalized
numbers may also be flushed to zero. This is intended to be a performance hint
and the OpenCL compiler can choose not to flush denorms to zero if the device
supports single precision (or double precision) denormalized numbers.

This option is ignored for single precision numbers if the device does not
support single precision denormalized numbers i.e. 'CL_FP_DENORM' bit is not set
in 'clGetDeviceSingleFPConfig'.

This option is ignored for double precision numbers if the device does not
support double precision or if it does support double precison but
'CL_FP_DENORM' bit is not set in 'clGetDeviceDoubleFPConfig'.

This flag only applies for scalar and vector single precision floating-point
variables and computations on these floating-point variables inside a
program. It does not apply to reading from or writing to image objects.

 * Optimization Options

These options control various sorts of optimizations. Turning on optimization
flags makes the compiler attempt to improve the performance and/or code size at
the expense of compilation time and possibly the ability to debug the program.

 [-cl-opt-disable] This option disables all optimizations. The default is
optimizations are enabled.

 [-cl-strict-aliasing] This option allows the compiler to assume the strictest
aliasing rules.

The following options control compiler behavior regarding floating-point
arithmetic. These options trade off between performance and correctness and must
be specifically enabled. These options are not turned on by default since it can
result in incorrect output for programs which depend on an exact implementation
of IEEE 754 rules/specifications for math functions.

 [-cl-mad-enable] Allow a * b + c to be replaced by a mad. The mad computes a *
b + c with reduced accuracy. For example, some OpenCL devices implement mad as
truncate the result of a * b before adding it to c.

 [-cl-no-signed-zeros] Allow optimizations for floating-point arithmetic that
ignore the signedness of zero. IEEE 754 arithmetic specifies the behavior of
distinct +0.0 and -0.0 values, which then prohibits simplification of
expressions such as x+0.0 or 0.0*x (even with -clfinite-math only). This option
implies that the sign of a zero result isn't significant.

 [-cl-unsafe-math-optimizations] Allow optimizations for floating-point
arithmetic that (a) assume that arguments and results are valid, (b) may violate
IEEE 754 standard and (c) may violate the OpenCL numerical compliance
requirements as defined in section 7.4 for single-precision floating-point,
section 9.3.9 for double-precision floating-point, and edge case behavior in
section 7.5. This option includes the -cl-no-signed-zeros and -cl-mad-enable
options.

 [-cl-finite-math-only] Allow optimizations for floating-point arithmetic that
assume that arguments and results are not NaNs or ±∞. This option may violate
the OpenCL numerical compliance requirements defined in in section 7.4 for
single-precision floating-point, section 9.3.9 for double-precision
floating-point, and edge case behavior in section 7.5.

 [-cl-fast-relaxed-math] Sets the optimization options -cl-finite-math-only and
-cl-unsafe-math-optimizations. This allows optimizations for floating-point
arithmetic that may violate the IEEE 754 standard and the OpenCL numerical
compliance requirements defined in the specification in section 7.4 for
single-precision floating-point, section 9.3.9 for double-precision
floating-point, and edge case behavior in section 7.5. This option causes the
preprocessor macro __FAST_RELAXED_MATH__ to be defined in the OpenCL program.

 * Options to Request or Suppress Warnings

Warnings are diagnostic messages that report constructions which are not
inherently erroneous but which are risky or suggest there may have been an
error. The following languageindependent options do not enable specific warnings
but control the kinds of diagnostics produced by the OpenCL compiler.

 [-w] Inhibit all warning messages.
 
 [-Werror] Make all warnings into errors.

clBuildProgram can throw the following 'CLError' exceptions when fails:

 * 'CL_INVALID_PROGRAM' if program is not a valid program object.

 * 'CL_INVALID_DEVICE' if OpenCL devices listed in device_list are not in the
list of devices associated with program.

 * 'CL_INVALID_BINARY' if program is created with
'clCreateWithProgramWithBinary' and devices listed in device_list do not have a
valid program binary loaded.

 * 'CL_INVALID_BUILD_OPTIONS' if the build options specified by options are
invalid.

 * 'CL_INVALID_OPERATION' if the build of a program executable for any of the
devices listed in device_list by a previous call to 'clBuildProgram' for program
has not completed.

 * 'CL_COMPILER_NOT_AVAILABLE' if program is created with
'clCreateProgramWithSource' and a compiler is not available
i.e. 'clGetDeviceCompilerAvailable' is set to 'False'.

 * 'CL_BUILD_PROGRAM_FAILURE' if there is a failure to build the program
executable. This error will be returned if 'clBuildProgram' does not return
until the build has completed.

 * 'CL_INVALID_OPERATION' if there are kernel objects attached to program.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.  
-}
clBuildProgram :: CLProgram -> [CLDeviceID] -> String -> IO ()
clBuildProgram prg devs opts = allocaArray ndevs $ \pdevs -> do
  pokeArray pdevs devs
  withCString opts $ \copts -> do
    whenSuccess (raw_clBuildProgram prg cndevs pdevs copts nullFunPtr nullPtr)
      $ return ()
    where
      ndevs = length devs
      cndevs = fromIntegral ndevs

#c
enum CLProgramInfo {
  cL_PROGRAM_REFERENCE_COUNT=CL_PROGRAM_REFERENCE_COUNT,
  cL_PROGRAM_CONTEXT=CL_PROGRAM_CONTEXT,
  cL_PROGRAM_NUM_DEVICES=CL_PROGRAM_NUM_DEVICES,
  cL_PROGRAM_DEVICES=CL_PROGRAM_DEVICES,
  cL_PROGRAM_SOURCE=CL_PROGRAM_SOURCE,
  cL_PROGRAM_BINARY_SIZES=CL_PROGRAM_BINARY_SIZES,
  cL_PROGRAM_BINARIES=CL_PROGRAM_BINARIES,
  };
#endc
{#enum CLProgramInfo {upcaseFirstLetter} #}

getProgramInfoSize :: CLProgram -> CLProgramInfo_ -> IO CSize
getProgramInfoSize prg infoid = alloca $ \(value_size :: Ptr CSize) -> do
  whenSuccess (raw_clGetProgramInfo prg infoid 0 nullPtr value_size)
    $ peek value_size
  
-- | Return the program reference count. The reference count returned should be
-- considered immediately stale. It is unsuitable for general use in
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetProgramInfo with
-- 'CL_PROGRAM_REFERENCE_COUNT'.
clGetProgramReferenceCount :: CLProgram -> IO CLuint
clGetProgramReferenceCount prg =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        raw_clGetProgramInfo prg infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_PROGRAM_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the context specified when the program object is created.
--
-- This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_CONTEXT'.
clGetProgramContext :: CLProgram -> IO CLContext
clGetProgramContext prg =
    wrapGetInfo (\(dat :: Ptr CLContext) ->
        raw_clGetProgramInfo prg infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_PROGRAM_CONTEXT
      size = fromIntegral $ sizeOf (nullPtr::CLContext)

-- | Return the number of devices associated with program.
--
-- This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_NUM_DEVICES'.
clGetProgramNumDevices :: CLProgram -> IO CLuint
clGetProgramNumDevices prg =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        raw_clGetProgramInfo prg infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_PROGRAM_NUM_DEVICES
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the list of devices associated with the program object. This can be
-- the devices associated with context on which the program object has been
-- created or can be a subset of devices that are specified when a progam object
-- is created using 'clCreateProgramWithBinary'.
--
-- This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_DEVICES'.
clGetProgramDevices :: CLProgram -> IO [CLDeviceID]
clGetProgramDevices prg = do
  size <- getProgramInfoSize prg infoid
  allocaArray (numElems size) $ \(buff :: Ptr CLDeviceID) -> do
    whenSuccess (raw_clGetProgramInfo prg infoid size (castPtr buff) nullPtr)
      $ peekArray (numElems size) buff
    where 
      infoid = getCLValue CL_PROGRAM_DEVICES
      numElems s = (fromIntegral s) `div` elemSize
      elemSize = sizeOf (nullPtr::CLDeviceID)

-- | Return the program source code specified by
-- 'clCreateProgramWithSource'. The source string returned is a concatenation of
-- all source strings specified to 'clCreateProgramWithSource' with a null
-- terminator. The concatenation strips any nulls in the original source
-- strings. The actual number of characters that represents the program source
-- code including the null terminator is returned in param_value_size_ret.
--
-- This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_SOURCE'.
clGetProgramSource :: CLProgram -> IO String
clGetProgramSource prg = do
  n <- getProgramInfoSize prg infoid
  allocaArray (fromIntegral n) $ \(buff :: CString) -> do
    whenSuccess (raw_clGetProgramInfo prg infoid n (castPtr buff) nullPtr)
      $ peekCString buff
    where 
      infoid = getCLValue CL_PROGRAM_SOURCE
  
-- | Returns an array that contains the size in bytes of the program binary for
-- each device associated with program. The size of the array is the number of
-- devices associated with program. If a binary is not available for a
-- device(s), a size of zero is returned.
--
-- This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_BINARY_SIZES'.
clGetProgramBinarySizes :: CLProgram -> IO [CSize]
clGetProgramBinarySizes prg = do
  size <- getProgramInfoSize prg infoid
  allocaArray (numElems size) $ \(buff :: Ptr CSize) -> do
    whenSuccess (raw_clGetProgramInfo prg infoid size (castPtr buff) nullPtr)
      $ peekArray (numElems size) buff
    where 
      infoid = getCLValue CL_PROGRAM_BINARY_SIZES
      numElems s = (fromIntegral s) `div` elemSize
      elemSize = sizeOf (0::CSize)

{-| Return the program binaries for all devices associated with program. For
each device in program, the binary returned can be the binary specified for the
device when program is created with 'clCreateProgramWithBinary' or it can be the
executable binary generated by 'clBuildProgram'. If program is created with
'clCreateProgramWithSource', the binary returned is the binary generated by
'clBuildProgram'. The bits returned can be an implementation-specific
intermediate representation (a.k.a. IR) or device specific executable bits or
both. The decision on which information is returned in the binary is up to the
OpenCL implementation.

To find out which device the program binary in the array refers to, use the
'clGetProgramDevices' query to get the list of devices. There is a one-to-one
correspondence between the array of data returned by 'clGetProgramBinaries' and
array of devices returned by 'clGetProgramDevices'.  

This function execute OpenCL clGetProgramInfo with 'CL_PROGRAM_BINARIES'.
-}
clGetProgramBinaries :: CLProgram -> IO [[Word8]]
clGetProgramBinaries prg = do
  sizes <- clGetProgramBinarySizes prg
  let numElems = length sizes
      size = fromIntegral $ numElems * elemSize
  buffers <- (mapM (mallocArray.fromIntegral) sizes) :: IO [Ptr Word8]
  ret <- withArray buffers $ \(parray :: Ptr (Ptr Word8)) -> do
    whenSuccess (raw_clGetProgramInfo prg infoid size (castPtr parray) nullPtr)
      $ zipWithM peekArray (map fromIntegral sizes) buffers
  mapM_ free buffers
  return ret
    where 
      infoid = getCLValue CL_PROGRAM_BINARIES
      elemSize = sizeOf (nullPtr::Ptr Word8)

-- -----------------------------------------------------------------------------
#c
enum CLProgramBuildInfo {
  cL_PROGRAM_BUILD_STATUS=CL_PROGRAM_BUILD_STATUS,
  cL_PROGRAM_BUILD_OPTIONS=CL_PROGRAM_BUILD_OPTIONS,
  cL_PROGRAM_BUILD_LOG=CL_PROGRAM_BUILD_LOG,
  };
#endc
{#enum CLProgramBuildInfo {upcaseFirstLetter} #}

getProgramBuildInfoSize :: CLProgram -> CLDeviceID -> CLProgramInfo_ -> IO CSize
getProgramBuildInfoSize prg device infoid = alloca $ \(val :: Ptr CSize) -> do
  whenSuccess (raw_clGetProgramBuildInfo prg device infoid 0 nullPtr val)
    $ peek val
  
-- | Returns the build status of program for a specific device as given by
-- device.
--
-- This function execute OpenCL clGetProgramBuildInfo with
-- 'CL_PROGRAM_BUILD_STATUS'.
clGetProgramBuildStatus :: CLProgram -> CLDeviceID -> IO CLBuildStatus
clGetProgramBuildStatus prg device =
    wrapGetInfo (\(dat :: Ptr CLBuildStatus_) ->
        raw_clGetProgramBuildInfo prg device infoid size (castPtr dat)) getEnumCL
    where 
      infoid = getCLValue CL_PROGRAM_BUILD_STATUS
      size = fromIntegral $ sizeOf (0::CLBuildStatus_)

-- | Return the build options specified by the options argument in
-- clBuildProgram for device. If build status of program for device is
-- 'CL_BUILD_NONE', an empty string is returned.
--
-- This function execute OpenCL clGetProgramBuildInfo with
-- 'CL_PROGRAM_BUILD_OPTIONS'.
clGetProgramBuildOptions :: CLProgram -> CLDeviceID -> IO String
clGetProgramBuildOptions prg device = do
  n <- getProgramBuildInfoSize prg device infoid
  allocaArray (fromIntegral n) $ \(buff :: CString) -> do
    whenSuccess (raw_clGetProgramBuildInfo prg device infoid n (castPtr buff) nullPtr)
      $ peekCString buff
    where 
      infoid = getCLValue CL_PROGRAM_BUILD_OPTIONS
  
-- | Return the build log when 'clBuildProgram' was called for device. If build
-- status of program for device is 'CL_BUILD_NONE', an empty string is returned.
--
-- This function execute OpenCL clGetProgramBuildInfo with
-- 'CL_PROGRAM_BUILD_LOG'.
clGetProgramBuildLog :: CLProgram -> CLDeviceID -> IO String
clGetProgramBuildLog prg device = do
  n <- getProgramBuildInfoSize prg device infoid
  allocaArray (fromIntegral n) $ \(buff :: CString) -> do
    whenSuccess (raw_clGetProgramBuildInfo prg device infoid n (castPtr buff) nullPtr)
      $ peekCString buff
    where 
      infoid = getCLValue CL_PROGRAM_BUILD_LOG
  
-- -----------------------------------------------------------------------------
{-| Creates a kernal object. A kernel is a function declared in a program. A
kernel is identified by the __kernel qualifier applied to any function in a
program. A kernel object encapsulates the specific __kernel function declared in
a program and the argument values to be used when executing this __kernel
function.

'clCreateKernel' returns a valid non-zero kernel object if the kernel object is
created successfully. Otherwise, it throws one of the following 'CLError'
exceptions:

 * 'CL_INVALID_PROGRAM' if program is not a valid program object.

 * 'CL_INVALID_PROGRAM_EXECUTABLE' if there is no successfully built executable
 for program.

 * 'CL_INVALID_KERNEL_NAME' if kernel_name is not found in program.

 * 'CL_INVALID_KERNEL_DEFINITION' if the function definition for __kernel
function given by kernel_name such as the number of arguments, the argument
types are not the same for all devices for which the program executable has been
built.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.  
-}
clCreateKernel :: CLProgram -> String -> IO CLKernel
clCreateKernel prg name = withCString name $ \cname -> wrapPError $ \perr -> do
  raw_clCreateKernel prg cname perr

{-| Creates kernel objects for all kernel functions in a program object. Kernel
objects are not created for any __kernel functions in program that do not have
the same function definition across all devices for which a program executable
has been successfully built.

Kernel objects can only be created once you have a program object with a valid
program source or binary loaded into the program object and the program
executable has been successfully built for one or more devices associated with
program. No changes to the program executable are allowed while there are kernel
objects associated with a program object. This means that calls to
'clBuildProgram' return 'CL_INVALID_OPERATION' if there are kernel objects
attached to a program object. The OpenCL context associated with program will be
the context associated with kernel. The list of devices associated with program
are the devices associated with kernel. Devices associated with a program object
for which a valid program executable has been built can be used to execute
kernels declared in the program object.

'clCreateKernelsInProgram' will return the kernel objects if the kernel objects
were successfully allocated, throws 'CL_INVALID_PROGRAM' if program is not a
valid program object, throws 'CL_INVALID_PROGRAM_EXECUTABLE' if there is no
successfully built executable for any device in program and throws
'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required by
the OpenCL implementation on the host.
-}
clCreateKernelsInProgram :: CLProgram -> IO [CLKernel]
clCreateKernelsInProgram prg = do
  n <- alloca $ \pn -> do
    whenSuccess (raw_clCreateKernelsInProgram prg 0 nullPtr pn)
      $ peek pn  
  allocaArray (fromIntegral n) $ \pks -> do
    whenSuccess (raw_clCreateKernelsInProgram prg n pks nullPtr)
      $ peekArray (fromIntegral n) pks

-- | Increments the program program reference count. 'clRetainKernel' returns
-- 'True' if the function is executed successfully. 'clCreateKernel' or
-- 'clCreateKernelsInProgram' do an implicit retain.
clRetainKernel :: CLKernel -> IO Bool
clRetainKernel krn = wrapCheckSuccess $ raw_clRetainKernel krn

-- | Decrements the kernel reference count. The kernel object is deleted once
-- the number of instances that are retained to kernel become zero and the
-- kernel object is no longer needed by any enqueued commands that use
-- kernel. 'clReleaseKernel' returns 'True' if the function is executed
-- successfully.
clReleaseKernel :: CLKernel -> IO Bool
clReleaseKernel krn = wrapCheckSuccess $ raw_clReleaseKernel krn

{-| Used to set the argument value for a specific argument of a kernel.

A kernel object does not update the reference count for objects such as memory,
sampler objects specified as argument values by 'clSetKernelArg', Users may not
rely on a kernel object to retain objects specified as argument values to the
kernel.

Implementations shall not allow 'CLKernel' objects to hold reference counts to
'CLKernel' arguments, because no mechanism is provided for the user to tell the
kernel to release that ownership right. If the kernel holds ownership rights on
kernel args, that would make it impossible for the user to tell with certainty
when he may safely release user allocated resources associated with OpenCL
objects such as the CLMem backing store used with 'CL_MEM_USE_HOST_PTR'.

'clSetKernelArg' throws one of the following 'CLError' exceptions when fails:

 * 'CL_INVALID_KERNEL' if kernel is not a valid kernel object.
 
 * 'CL_INVALID_ARG_INDEX' if arg_index is not a valid argument index.

 * 'CL_INVALID_ARG_VALUE' if arg_value specified is NULL for an argument that is
not declared with the __local qualifier or vice-versa.

 * 'CL_INVALID_MEM_OBJECT' for an argument declared to be a memory object when
the specified arg_value is not a valid memory object.

 * 'CL_INVALID_SAMPLER' for an argument declared to be of type sampler_t when
the specified arg_value is not a valid sampler object.

 * 'CL_INVALID_ARG_SIZE' if arg_size does not match the size of the data type
for an argument that is not a memory object or if the argument is a memory
object and arg_size != sizeof(cl_mem) or if arg_size is zero and the argument is
declared with the __local qualifier or if the argument is a sampler and arg_size
!= sizeof(cl_sampler).  
-}
clSetKernelArg :: Integral a => CLKernel -> CLuint -> a -> Ptr b -> IO ()
clSetKernelArg krn idx sz pval = do
  whenSuccess (raw_clSetKernelArg krn idx (fromIntegral sz) (castPtr pval))
    $ return ()

-- | Wrap function of `clSetKernelArg` with Storable data.
clSetKernelArgSto :: Storable a => CLKernel -> CLuint -> a -> IO ()
clSetKernelArgSto krn idx val = with val $ \pval -> do
  whenSuccess (raw_clSetKernelArg krn idx (fromIntegral . sizeOf $ val) (castPtr pval))
    $ return ()

#c
enum CLKernelInfo {
  cL_KERNEL_FUNCTION_NAME=CL_KERNEL_FUNCTION_NAME,
  cL_KERNEL_NUM_ARGS=CL_KERNEL_NUM_ARGS,
  cL_KERNEL_REFERENCE_COUNT=CL_KERNEL_REFERENCE_COUNT,
  cL_KERNEL_CONTEXT=CL_KERNEL_CONTEXT,
  cL_KERNEL_PROGRAM=CL_KERNEL_PROGRAM
  };
#endc
{#enum CLKernelInfo {upcaseFirstLetter} #}

getKernelInfoSize :: CLKernel -> CLKernelInfo_ -> IO CSize
getKernelInfoSize krn infoid = alloca $ \(val :: Ptr CSize) -> do
  whenSuccess (raw_clGetKernelInfo krn infoid 0 nullPtr val)
    $ peek val
  
-- | Return the kernel function name.
--
-- This function execute OpenCL clGetKernelInfo with 'CL_KERNEL_FUNCTION_NAME'.
clGetKernelFunctionName :: CLKernel -> IO String
clGetKernelFunctionName krn = do
  n <- getKernelInfoSize krn infoid
  allocaArray (fromIntegral n) $ \(buff :: CString) -> do
    whenSuccess (raw_clGetKernelInfo krn infoid n (castPtr buff) nullPtr)
      $ peekCString buff
    where 
      infoid = getCLValue CL_KERNEL_FUNCTION_NAME

-- | Return the number of arguments to kernel.
--
-- This function execute OpenCL clGetKernelInfo with 'CL_KERNEL_NUM_ARGS'.
clGetKernelNumArgs :: CLKernel -> IO CLuint
clGetKernelNumArgs krn =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        raw_clGetKernelInfo krn infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_KERNEL_NUM_ARGS
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the kernel reference count. The reference count returned should be
-- considered immediately stale. It is unsuitable for general use in
-- applications. This feature is provided for identifying memory leaks.
--
-- This function execute OpenCL clGetKernelInfo with
-- 'CL_KERNEL_REFERENCE_COUNT'.
clGetKernelReferenceCount :: CLKernel -> IO CLuint
clGetKernelReferenceCount krn =
    wrapGetInfo (\(dat :: Ptr CLuint) ->
        raw_clGetKernelInfo krn infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_KERNEL_REFERENCE_COUNT
      size = fromIntegral $ sizeOf (0::CLuint)

-- | Return the context associated with kernel.
--
-- This function execute OpenCL clGetKernelInfo with 'CL_KERNEL_CONTEXT'.
clGetKernelContext :: CLKernel -> IO CLContext
clGetKernelContext krn =
    wrapGetInfo (\(dat :: Ptr CLContext) ->
        raw_clGetKernelInfo krn infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_KERNEL_CONTEXT
      size = fromIntegral $ sizeOf (nullPtr::CLContext)

-- | Return the program object associated with kernel.
--
-- This function execute OpenCL clGetKernelInfo with 'CL_KERNEL_PROGRAM'.
clGetKernelProgram :: CLKernel -> IO CLProgram
clGetKernelProgram krn =
    wrapGetInfo (\(dat :: Ptr CLProgram) ->
        raw_clGetKernelInfo krn infoid size (castPtr dat)) id
    where 
      infoid = getCLValue CL_KERNEL_PROGRAM
      size = fromIntegral $ sizeOf (nullPtr::CLProgram)


#c
enum CLKernelGroupInfo {
  cL_KERNEL_WORK_GROUP_SIZE=CL_KERNEL_WORK_GROUP_SIZE,
  cL_KERNEL_COMPILE_WORK_GROUP_SIZE=CL_KERNEL_COMPILE_WORK_GROUP_SIZE,
  cL_KERNEL_LOCAL_MEM_SIZE=CL_KERNEL_LOCAL_MEM_SIZE,
  };
#endc
{#enum CLKernelGroupInfo {upcaseFirstLetter} #}

-- | This provides a mechanism for the application to query the work-group size
-- that can be used to execute a kernel on a specific device given by
-- device. The OpenCL implementation uses the resource requirements of the
-- kernel (register usage etc.) to determine what this work-group size should
-- be.
--
-- This function execute OpenCL clGetKernelWorkGroupInfo with
-- 'CL_KERNEL_WORK_GROUP_SIZE'.
clGetKernelWorkGroupSize :: CLKernel -> CLDeviceID -> IO CSize
clGetKernelWorkGroupSize krn device =
    wrapGetInfo (\(dat :: Ptr CSize) ->
        raw_clGetKernelWorkGroupInfo krn device infoid size (castPtr dat)) id
    where
      infoid = getCLValue CL_KERNEL_WORK_GROUP_SIZE
      size = fromIntegral $ sizeOf (0::CSize)

-- | Returns the work-group size specified by the __attribute__((reqd_work_gr
-- oup_size(X, Y, Z))) qualifier. See Function Qualifiers. If the work-group
-- size is not specified using the above attribute qualifier (0, 0, 0) is
-- returned.
--
-- This function execute OpenCL clGetKernelWorkGroupInfo with
-- 'CL_KERNEL_COMPILE_WORK_GROUP_SIZE'.
clGetKernelCompileWorkGroupSize :: CLKernel -> CLDeviceID -> IO [CSize]
clGetKernelCompileWorkGroupSize krn device = do
  allocaArray num $ \(buff :: Ptr CSize) -> do
    whenSuccess (raw_clGetKernelWorkGroupInfo krn device infoid size (castPtr buff) nullPtr)
      $ peekArray num buff
    where 
      infoid = getCLValue CL_KERNEL_COMPILE_WORK_GROUP_SIZE
      num = 3
      elemSize = fromIntegral $ sizeOf (0::CSize)
      size = fromIntegral $ num * elemSize


-- | Returns the amount of local memory in bytes being used by a kernel. This
-- includes local memory that may be needed by an implementation to execute the
-- kernel, variables declared inside the kernel with the __local address
-- qualifier and local memory to be allocated for arguments to the kernel
-- declared as pointers with the __local address qualifier and whose size is
-- specified with 'clSetKernelArg'.
--
-- If the local memory size, for any pointer argument to the kernel declared
-- with the __local address qualifier, is not specified, its size is assumed to
-- be 0.
--
-- This function execute OpenCL clGetKernelWorkGroupInfo with
-- 'CL_KERNEL_LOCAL_MEM_SIZE'.
clGetKernelLocalMemSize :: CLKernel -> CLDeviceID -> IO CLulong
clGetKernelLocalMemSize krn device =
    wrapGetInfo (\(dat :: Ptr CLulong) ->
        raw_clGetKernelWorkGroupInfo krn device infoid size (castPtr dat)) id
    where
      infoid = getCLValue CL_KERNEL_LOCAL_MEM_SIZE
      size = fromIntegral $ sizeOf (0::CLulong)
-- -----------------------------------------------------------------------------
