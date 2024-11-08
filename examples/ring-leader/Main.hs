{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Main where

import Choreography
import Choreography.Choreo
import Choreography.Network
import Data.Proxy
import GHC.TypeLits (KnownSymbol)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Environment
import Control.Selective
import Ring




main :: IO ()
main = do
  print (allLocs (ringLeader ring))
  print (communicationMap (ringLeader ring))
  [loc] <- getArgs
  putStrLn "Please input a label:"
  label <- read <$> getLine
--  runNetwork config "A" ($$(runR $ stagedEpp ifBoolTest "A"))
--  runStateT (runChoreography config (ringLeader ring) loc) label
  case loc of
    "A" -> runStateT (runNetwork config "A" $$(runR $ stagedEpp (ringLeader ring) "A")) label
    "B" -> runStateT (runNetwork config "B" $$(runR $ stagedEpp (ringLeader ring) "B")) label
    "C" -> runStateT (runNetwork config "C" $$(runR $ stagedEpp (ringLeader ring) "C")) label
    "D" -> runStateT (runNetwork config "D" $$(runR $ stagedEpp (ringLeader ring) "D")) label
  return ()
  where
    config = mkHttpConfig [ ("A", ("localhost", 4242))
                          , ("B", ("localhost", 4343))
                          , ("C", ("localhost", 4444))
                          , ("D", ("localhost", 4545))
                          ]



