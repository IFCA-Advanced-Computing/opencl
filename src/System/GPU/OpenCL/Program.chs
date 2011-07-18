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
module System.GPU.OpenCL.Program(  
  -- * Types
  CLProgram,
  -- * Functions
  clCreateProgramWithSource, clRetainProgram, clReleaseProgram, 
  clUnloadCompiler, clBuildProgram
  ) where

-- -----------------------------------------------------------------------------
import Foreign
import Foreign.C.Types
import Foreign.C.String( CString, withCString, newCString )
import System.GPU.OpenCL.Types( 
  CLint, CLuint, CLProgram, CLContext, CLDeviceID, CLError(..), 
  wrapCheckSuccess, wrapPError )

#include <CL/cl.h>

-- -----------------------------------------------------------------------------
type BuildCallback = CLProgram -> Ptr () -> IO ()
foreign import ccall "clCreateProgramWithSource" raw_clCreateProgramWithSource :: 
  CLContext -> CLuint -> Ptr CString -> Ptr CSize -> Ptr CLint -> IO CLProgram
foreign import ccall "clCreateProgramWithBinary" raw_clCreateProgramWithBinary :: 
  CLContext -> CLuint -> Ptr CLDeviceID -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CLint -> Ptr CLint -> IO CLProgram
foreign import ccall "clRetainProgram" raw_clRetainProgram :: 
  CLProgram -> IO CLint
foreign import ccall "clReleaseProgram" raw_clReleaseProgram :: 
  CLProgram -> IO CLint
foreign import ccall "clBuildProgram" raw_clBuildProgram :: 
  CLProgram -> CLuint -> Ptr CLDeviceID -> CString -> FunPtr BuildCallback -> Ptr () -> IO CLint
foreign import ccall "clUnloadCompiler" raw_clUnloadCompiler :: 
  IO CLint
foreign import ccall "clGetProgramInfo" raw_clGetProgramInfo :: 
  CLProgram -> CLuint -> CSize -> Ptr () -> Ptr CSize -> IO CLint
foreign import ccall "clGetProgramBuildInfo"  raw_clGetProgramBuildInfo :: 
  CLProgram -> CLuint -> CSize -> Ptr () -> Ptr CSize -> IO CLint

-- -----------------------------------------------------------------------------
clCreateProgramWithSource :: CLContext -> String -> IO (Either CLError CLProgram)
clCreateProgramWithSource ctx source = wrapPError $ \perr -> do
  let strings = lines source
      count = fromIntegral $ length strings
  cstrings <- mapM newCString strings
  prog <- withArray cstrings $ \srcArray -> do
    raw_clCreateProgramWithSource ctx count srcArray nullPtr perr
  mapM_ free cstrings
  return prog
  
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
in 'CL_DEVICE_SINGLE_FP_CONFIG'.

This option is ignored for double precision numbers if the device does not
support double precision or if it does support double precison but
'CL_FP_DENORM' bit is not set in 'CL_DEVICE_DOUBLE_FP_CONFIG'.

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

clBuildProgram returns the following errors when fails:

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
i.e. 'CL_DEVICE_COMPILER_AVAILABLE' specified in the table of OpenCL Device
Queries for 'clGetDeviceCompilerAvailable' is set to 'False'.

 * 'CL_BUILD_PROGRAM_FAILURE' if there is a failure to build the program
executable. This error will be returned if 'clBuildProgram' does not return
until the build has completed.

 * 'CL_INVALID_OPERATION' if there are kernel objects attached to program.

 * 'CL_OUT_OF_HOST_MEMORY' if there is a failure to allocate resources required
by the OpenCL implementation on the host.  
-}
clBuildProgram :: CLProgram -> [CLDeviceID] -> String -> IO (Either CLError ())
clBuildProgram prg devs opts = allocaArray ndevs $ \pdevs -> do
  pokeArray pdevs devs
  withCString opts $ \copts -> do
    errcode <- raw_clBuildProgram prg cndevs pdevs copts nullFunPtr nullPtr
    if errcode == (fromIntegral . fromEnum $ CL_SUCCESS)
      then return $ Right ()
      else return . Left . toEnum . fromIntegral $ errcode
    where
      ndevs = length devs
      cndevs = fromIntegral ndevs

-- -----------------------------------------------------------------------------
