{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE TemplateHaskell  #-}

module A where

import Choreography
import Data.Proxy
import GHC.TypeLits (KnownSymbol)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Environment
import Control.Selective
nodeA :: Proxy "A"
nodeA = Proxy

nodeB :: Proxy "B"
nodeB = Proxy

simplest =
--  Loop $ \r -> Eff nodeA (V (print ()) [|| print () ||]) **> r

  Loop (\r ->
    let labelLeft = (nodeA, Eff nodeA (V (pure ()) [|| pure () ||]), V (\_ -> pure 1) [|| \_ -> pure 1 ||]) ~~> nodeB
    in LocalAp nodeB (Eff nodeB (V (pure print) [|| pure (print @Int) ||])) labelLeft
     **> r )
