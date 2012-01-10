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
module Control.Parallel.OpenCL( 
  -- * Basic Types
  CLError(..), CLint, CLuint, CLulong,
  -- * Modules
  module Control.Parallel.OpenCL.Query,
  module Control.Parallel.OpenCL.Context,
  module Control.Parallel.OpenCL.CommandQueue, 
  module Control.Parallel.OpenCL.Memory,
  module Control.Parallel.OpenCL.Event,
  module Control.Parallel.OpenCL.Program
  )
       where

-- -----------------------------------------------------------------------------
import Control.Parallel.OpenCL.Query
import Control.Parallel.OpenCL.Context
import Control.Parallel.OpenCL.CommandQueue
import Control.Parallel.OpenCL.Memory
import Control.Parallel.OpenCL.Event
import Control.Parallel.OpenCL.Program
import Control.Parallel.OpenCL.Types( 
  CLError(..), CLint, CLuint, CLulong )

-- -----------------------------------------------------------------------------
