module System.GPU.OpenCL.Types( ErrorCode(..), CLPlatformID, CLuint, CLint ) where

import Foreign( Ptr )
import Foreign.C.Types( CUInt, CInt )

data PlatformIDc = PlatformIDc

type CLPlatformID = Ptr PlatformIDc

type CLint = CInt
type CLuint = CUInt

newtype ErrorCode = ErrorCode CLint deriving( Eq )
