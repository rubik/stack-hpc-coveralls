module SHC (
    Config(..)
  , ConversionType(..)
  , PostResult(..)
  , generateCoverallsFromTix
  , sendData
  , readCoverageResult
  , getHpcDir
  , getMixDir
  , getGitInfo
  )
    where


import SHC.Types
import SHC.Api
import SHC.Utils
import SHC.Coveralls
