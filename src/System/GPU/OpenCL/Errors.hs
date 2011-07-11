module System.GPU.OpenCL.Errors( ErrorCode(..), clSuccess ) where

import System.GPU.OpenCL.Types( ErrorCode(..) )

clSuccess :: ErrorCode
clSuccess = ErrorCode (0)
