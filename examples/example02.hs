import System.GPU.OpenCL  
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat )
import Foreign.Marshal.Array( peekArray, withArray )
import Data.List( foldl' )
import Control.Monad( forM_, forM )

myTry :: IO (Either CLError b) -> IO b
myTry f = do
  v <- f
  case v of
    Left err -> error . show $ err
    Right res -> return res

programSource :: String
programSource = "__kernel void duparray(__global float *in, __global float *out ){\n  int id = get_global_id(0);\n  out[id] = 2*in[id] + id;\n}"

sumres :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
sumres (t1,t2,t3) (u1,u2,u3) = (t1+u1,t2+u2,t3+u3)

main :: IO ()
main = do
  -- Initialize OpenCL
  (platform:_) <- myTry $ clGetPlatformIDs
  (dev:_) <- myTry $ clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- myTry $ clCreateContext [dev] print
  q <- myTry $ clCreateCommandQueue context dev [CL_QUEUE_PROFILING_ENABLE]
  
  -- Initialize Kernel
  program <- myTry $ clCreateProgramWithSource context programSource
  myTry $ clBuildProgram program [dev] ""
  kernel <- myTry $ clCreateKernel program "duparray"
  
  -- run tests
  forM_ [100,200..30000] $ \s -> do
    let original = [0 .. s] :: [CFloat]
        n = 50 :: Double
    res <- forM [0..n] $ \_ -> do
      (t1,t2,t3,_) <- executeArray original context q kernel
      return (t1,t2,t3)
    
    let (t1,t2,t3) = foldl' sumres (0,0,0) res
        
    putStrLn $ show s ++ "\t" ++ show (fromIntegral t1/n) ++ "\t" ++ show (fromIntegral t2/n) ++ "\t" ++ show (fromIntegral t3/n)

  return ()

executeArray :: [CFloat] -> CLContext -> CLCommandQueue -> CLKernel -> IO (CLulong, CLulong, CLulong, [CFloat])
executeArray original ctx q krn = withArray original $ \input -> do
  mem_in <- myTry $ clCreateBuffer ctx [CL_MEM_READ_ONLY] (vecSize, nullPtr)  
  mem_out <- myTry $ clCreateBuffer ctx [CL_MEM_WRITE_ONLY] (vecSize, nullPtr)

  myTry $ clSetKernelArg krn 0 mem_in
  myTry $ clSetKernelArg krn 1 mem_out
  
  -- Put Input
  eventWrite <- myTry $ clEnqueueWriteBuffer q mem_in True 0 vecSize (castPtr input) []
  
  -- Execute Kernel
  eventExec <- myTry $ clEnqueueNDRangeKernel q krn [length original] [1] [eventWrite]
  
  -- Get Result
  eventRead <- myTry $ clEnqueueReadBuffer q mem_out True 0 vecSize (castPtr input) [eventExec]
    
  _ <- clWaitForEvents [eventRead]
  
  t_start0 <- myTry $ clGetEventProfilingInfo eventWrite CL_PROFILING_COMMAND_START
  t_end0 <- myTry $ clGetEventProfilingInfo eventWrite CL_PROFILING_COMMAND_END
  let t_write = t_end0 - t_start0
      
  t_start1 <- myTry $ clGetEventProfilingInfo eventExec CL_PROFILING_COMMAND_START
  t_end1 <- myTry $ clGetEventProfilingInfo eventExec CL_PROFILING_COMMAND_END
  let t_exec = t_end1 - t_start1
  
  t_start2 <- myTry $ clGetEventProfilingInfo eventRead CL_PROFILING_COMMAND_START
  t_end2 <- myTry $ clGetEventProfilingInfo eventRead CL_PROFILING_COMMAND_END
  let t_read = t_end2 - t_start2
  
  result <- peekArray (length original) input
  
  return (t_write,t_exec,t_read,result)
    where
      elemSize = sizeOf (0 :: CFloat)
      vecSize = elemSize * length original
