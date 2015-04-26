{-# LANGUAGE CPP#-}
module Paths (getStaticDir) where

import Control.Monad
import System.FilePath

#if CABAL
-- using cabal
import qualified Paths_snooker_statistics (getDataDir)

getStaticDir :: IO FilePath
getStaticDir = (</> "data") `liftM` Paths_snooker_statistics.getDataDir



#else
-- using GHCi

getStaticDir :: IO FilePath
getStaticDir = return "data/"

#endif
