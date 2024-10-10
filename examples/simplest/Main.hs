{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fspecialise-aggressively -flate-specialise -ddump-splices -ddump-simpl -dno-typeable-binds -dsuppress-uniques -dsuppress-all #-}

module Main where

import Choreography
import Choreography.Network
import Choreography.Choreo
import Data.Proxy
import GHC.TypeLits (KnownSymbol)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Environment
import Control.Selective
import A

foo = let res = print () *> res in res



main :: IO ()
main = do
  [loc] <- getArgs
--  runChoreography config simplest loc
--  runChoreo simplest
  case loc of
--    "A" -> runNetwork config "A" $$(runR $ stagedEpp simplest "A")
--    "B" -> runNetwork config "B" $$(runR $ stagedEpp simplest "B")
      _ -> $$(staged simplest)
  return ()
  where

config = mkHttpConfig [ ("A", ("localhost", 4242))
                      , ("B", ("localhost", 4343))
                      ]






