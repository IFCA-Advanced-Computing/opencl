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
module System.GPU.OpenCL( 
  -- * Basic Types
  CLError(..), CLint, CLuint, CLulong,
  -- * Modules
  module System.GPU.OpenCL.Query,
  module System.GPU.OpenCL.Context,
  module System.GPU.OpenCL.CommandQueue, 
  module System.GPU.OpenCL.Memory,
  module System.GPU.OpenCL.Event,
  module System.GPU.OpenCL.Program )
       where

-- -----------------------------------------------------------------------------
import System.GPU.OpenCL.Query
import System.GPU.OpenCL.Context
import System.GPU.OpenCL.CommandQueue
import System.GPU.OpenCL.Memory
import System.GPU.OpenCL.Event
import System.GPU.OpenCL.Program
import System.GPU.OpenCL.Types( 
  CLError(..), CLint, CLuint, CLulong )

-- -----------------------------------------------------------------------------
