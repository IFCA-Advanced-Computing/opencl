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
import System.GPU.OpenCL  
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat )
import Foreign.Marshal.Array( newArray, peekArray )

myTry :: IO (Either CLError b) -> IO b
myTry f = do
  v <- f
  case v of
    Left err -> error . show $ err
    Right res -> return res

programSource1 :: String
programSource1 = "__kernel void duparray(__global float *in, __global float *out ){\n  int id = get_global_id(0);\n  out[id] = 2*in[id];\n}"

programSource2 :: String
programSource2 = "__kernel void triparray(__global float *in, __global float *out ){\n  int id = get_global_id(0);\n  out[id] = 3*in[id];\n}"

main :: IO ()
main = do
  -- Initialize OpenCL
  (platform:_) <- myTry $ clGetPlatformIDs
  (dev:_) <- myTry $ clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- myTry $ clCreateContext [dev] print
  q <- myTry $ clCreateCommandQueue context dev []
  
  -- Initialize Kernels
  program1 <- myTry $ clCreateProgramWithSource context programSource1
  myTry $ clBuildProgram program1 [dev] ""
  kernel1 <- myTry $ clCreateKernel program1 "duparray"
  kernel3 <- myTry $ clCreateKernel program1 "duparray"
  
  program2 <- myTry $ clCreateProgramWithSource context programSource2
  myTry $ clBuildProgram program2 [dev] ""
  kernel2 <- myTry $ clCreateKernel program2 "triparray"
  
  -- Initialize parameters
  let original = [0 .. 10] :: [CFloat]
      elemSize = sizeOf (0 :: CFloat)
      vecSize = elemSize * length original
  putStrLn $ "Original array = " ++ show original
  input  <- newArray original

  mem_in <- myTry $ clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (vecSize, castPtr input)  
  mem_mid <- myTry $ clCreateBuffer context [CL_MEM_READ_WRITE] (vecSize, nullPtr)
  mem_out1 <- myTry $ clCreateBuffer context [CL_MEM_WRITE_ONLY] (vecSize, nullPtr)
  mem_out2 <- myTry $ clCreateBuffer context [CL_MEM_WRITE_ONLY] (vecSize, nullPtr)

  myTry $ clSetKernelArg kernel1 0 mem_in
  myTry $ clSetKernelArg kernel1 1 mem_mid
  
  myTry $ clSetKernelArg kernel2 0 mem_mid
  myTry $ clSetKernelArg kernel2 1 mem_out1
  
  myTry $ clSetKernelArg kernel3 0 mem_mid
  myTry $ clSetKernelArg kernel3 1 mem_out2
  
  -- Execute Kernels
  eventExec1 <- myTry $ clEnqueueNDRangeKernel q kernel1 [length original] [1] []
  eventExec2 <- myTry $ clEnqueueNDRangeKernel q kernel2 [length original] [1] [eventExec1]
  eventExec3 <- myTry $ clEnqueueNDRangeKernel q kernel3 [length original] [1] [eventExec1]
  
  -- Get Result
  eventRead <- myTry $ clEnqueueReadBuffer q mem_out1 True 0 vecSize (castPtr input) [eventExec2,eventExec3]
  
  result <- peekArray (length original) input
  putStrLn $ "Result array 1 = " ++ show result

  eventRead <- myTry $ clEnqueueReadBuffer q mem_out2 True 0 vecSize (castPtr input) [eventExec2,eventExec3]
  
  result <- peekArray (length original) input
  putStrLn $ "Result array 2 = " ++ show result

  return ()
