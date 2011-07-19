import System.GPU.OpenCL  
import Foreign( castPtr, nullPtr )
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
  (platform:_) <- clGetPlatformIDs
  (dev:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- myTry $ clCreateContext [dev] print
  q <- myTry $ clCreateCommandQueue context dev []
  
  -- Initializa Kernel
  program <- myTry $ clCreateProgramWithSource context programSource
  myTry $ clBuildProgram program [dev] ""
  kernel <- myTry $ clCreateKernel program "duparray"
  
  -- Execute Kernel
  let original = [0 .. 20] :: [CFloat]
  putStrLn $ "Original array = " ++ show original
  input  <- newArray original

  mem_in <- myTry $ clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (fromIntegral $ 4*length original, castPtr input)  
  mem_out <- myTry $ clCreateBuffer context [CL_MEM_WRITE_ONLY] (fromIntegral $ 4*length original, nullPtr)

  myTry $ clSetKernelArg kernel 0 mem_in
  myTry $ clSetKernelArg kernel 1 mem_out
  
  eventExec <- myTry $ clEnqueueNDRangeKernel q kernel [fromIntegral $ length original] [1] []
  
  -- Get Result
  eventRead <- myTry $ clEnqueueReadBuffer q mem_out True 0 (fromIntegral $ 4*length original) (castPtr input) [eventExec]
  
  result <- peekArray (length original) input
  putStrLn $ "Result array = " ++ show result

  return ()
