{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines the interface to HasChor. The client of the library is
-- highly recommended to only use constructs exported by this module.
module Choreography (
  -- * Locations and Located Values
  LocTm,
  LocTy,
  type (@),

  -- * The Choreo monad
  Choreo,
  V(..),
  -- ** Choreo operations
  locally,
  (~>),
  (~~>),
  condBool,
  FreeApplicative(..),
  (**>),
--  cond',

  -- * Message transport backends
  -- ** The HTTP backend
  Host,
  Port,
  HttpConfig,
   mkHttpConfig,
   ifBool,

  -- * Running choreographies
  runChoreo,
  runChoreography
  ) where

import Choreography.Location
import Choreography.Choreo
import Choreography.Network
import Choreography.Network.Http
--import Choreography.Network.Local
import Control.Monad.IO.Class
import Data.Proxy
import Language.Haskell.TH.Syntax

-- | Run a choreography with a message transport backend.
runChoreography :: (Lift config, Backend config, MonadIO m) => config -> Choreo m a -> LocTm -> m a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)

runChoreographyStaged cfg choreo l = [|| runNetwork cfg l $$(runR $ stagedEpp choreo l) ||]

