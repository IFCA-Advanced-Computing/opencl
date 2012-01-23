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
import Control.Parallel.OpenCL
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat )
import Foreign.Marshal.Array( newArray, peekArray )

programSource :: String
programSource = "__kernel void duparray(__global float *in, __global float *out ){\n  int id = get_global_id(0);\n  out[id] = 2*in[id];\n}"

main :: IO ()
main = do
  -- Initialize OpenCL
  (platform:_) <- clGetPlatformIDs
  (dev:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [] [dev] print
  q <- clCreateCommandQueue context dev []
  
  -- Compile and get binaries
  program <- clCreateProgramWithSource context programSource
  clBuildProgram program [dev] ""
  bins <- clGetProgramBinaries program
  
  -- Create kernel from binaries
  (program2,_) <- clCreateProgramWithBinary context [dev] bins
  clBuildProgram program2 [dev] ""
  kernel <- clCreateKernel program2 "duparray"
  
  -- Initialize parameters
  let original = [0 .. 20] :: [CFloat]
      elemSize = sizeOf (0 :: CFloat)
      vecSize = elemSize * length original
  putStrLn $ "Original array = " ++ show original
  input  <- newArray original

  mem_in <- clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (vecSize, castPtr input)  
  mem_out <- clCreateBuffer context [CL_MEM_WRITE_ONLY] (vecSize, nullPtr)

  clSetKernelArgSto kernel 0 mem_in
  clSetKernelArgSto kernel 1 mem_out
  
  -- Execute Kernel
  eventExec <- clEnqueueNDRangeKernel q kernel [length original] [1] []
  
  -- Get Result
  eventRead <- clEnqueueReadBuffer q mem_out True 0 vecSize (castPtr input) [eventExec]
  
  result <- peekArray (length original) input
  putStrLn $ "Result array = " ++ show result

  return ()
