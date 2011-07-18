import System.GPU.OpenCL  
import Foreign( castPtr, nullPtr )
import Foreign.C.Types( CFloat )
import Foreign.Marshal.Array( newArray, peekArray )

myTry f = do
  v <- f
  case v of
    Left err -> do
      print err
      error ""
    Right res -> return res

programSource :: String
programSource = "__kernel void duparray(__global float *in, __global float *out ){\n  int id = get_global_id(0);\n  out[id] = 2*in[id];\n}"

main = do
  (platform:_) <- clGetPlatformIDs
  putStrLn $ "platform = " ++ show platform
  (dev:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  putStrLn $ "device = " ++ show dev
  Just context <- clCreateContext [dev] print
  putStrLn $ "context = " ++ show context
  
  q <- myTry $ clCreateCommandQueue context dev []
  putStrLn $ "queue = " ++ show q
  
  program <- myTry $ clCreateProgramWithSource context programSource
  putStrLn $ "program = " ++ show program
  
  myTry $ clBuildProgram program [dev] ""
  
  let original = [0,1,2,3,4,5,6,7,8,9] :: [CFloat]
  input  <- newArray original

  mem_in <- myTry $ clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (fromIntegral $ 4*length original, castPtr input)
  putStrLn $ "mem_in = " ++ show mem_in
  
  mem_out <- myTry $ clCreateBuffer context [CL_MEM_WRITE_ONLY] (fromIntegral $ 4*length original, nullPtr)
  putStrLn $ "mem_out = " ++ show mem_out

  eventRead <- myTry $ clEnqueueReadBuffer q mem_out True 0 (fromIntegral $ 4*length original) (castPtr input) []
  print eventRead
  
  result <- peekArray (length original) input
  print result

  return (platform,dev,context,program)
