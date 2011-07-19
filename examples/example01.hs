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

programSource :: String
programSource = "__kernel void duparray(__global float *in, __global float *out ){\n  int id = get_global_id(0);\n  out[id] = 2*in[id];\n}"

main :: IO ()
main = do
  -- Initialize OpenCL
  (platform:_) <- myTry $ clGetPlatformIDs
  (dev:_) <- myTry $ clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- myTry $ clCreateContext [dev] print
  q <- myTry $ clCreateCommandQueue context dev []
  
  -- Initialize Kernel
  program <- myTry $ clCreateProgramWithSource context programSource
  myTry $ clBuildProgram program [dev] ""
  kernel <- myTry $ clCreateKernel program "duparray"
  
  -- Initialize parameters
  let original = [0 .. 20] :: [CFloat]
      elemSize = sizeOf (0 :: CFloat)
      vecSize = elemSize * length original
  putStrLn $ "Original array = " ++ show original
  input  <- newArray original

  mem_in <- myTry $ clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (vecSize, castPtr input)  
  mem_out <- myTry $ clCreateBuffer context [CL_MEM_WRITE_ONLY] (vecSize, nullPtr)

  myTry $ clSetKernelArg kernel 0 mem_in
  myTry $ clSetKernelArg kernel 1 mem_out
  
  -- Execute Kernel
  eventExec <- myTry $ clEnqueueNDRangeKernel q kernel [length original] [1] []
  
  -- Get Result
  eventRead <- myTry $ clEnqueueReadBuffer q mem_out True 0 vecSize (castPtr input) [eventExec]
  
  result <- peekArray (length original) input
  putStrLn $ "Result array = " ++ show result

  return ()
