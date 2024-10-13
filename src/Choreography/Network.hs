-- | This module defines the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
{-# LANGUAGE RankNTypes #-}
module Choreography.Network where

import Choreography.Location
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Selective

-- * The Network monad

-- | Effect signature for the `Network` monad.
data NetworkSig m a where
  -- | Local computation.
  Run :: m a
      -> NetworkSig m a
  -- | Sending.
  Send :: Show a
       => a
       -> LocTm
       -> NetworkSig m ()
  -- | Receiving.
  Recv :: Read a
       => LocTm
       -> NetworkSig m a
  -- | Broadcasting.
  BCast :: Show a
        => a
        -> NetworkSig m ()

class NetworkT n m where
  run_ :: m a -> n m a
  send_ :: (Show a) => a -> LocTm -> n m ()
  recv_ :: (Read a) => LocTm -> n m a
  broadcast_ :: (Show a) => a -> n m ()

run x = Network (run_ x)
send x l = Network (send_ x l)
recv :: forall a m . (Read a) => LocTm -> Network m a
recv l = Network (recv_ l)
broadcast a = Network (broadcast_ a)



-- | Monad that represents network programs.
data Network m a = Network { runN :: forall n . (Monad (n m), NetworkT n m) => (n m a) }

instance Functor (Network m) where
  fmap f (Network a) = Network (fmap f a)

instance Applicative (Network m) where
  pure x = Network (pure x)
  (Network fa) <*> ~(Network a) = Network (fa <*> a)

instance Monad (Network m) where
  (Network a) >>= f = Network (a >>= \x -> case f x of Network n -> n)

instance Selective (Network m) where
  select = selectM

-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
class Backend c where
  runNetwork :: (MonadIO m) => c -> LocTm -> Network m a -> m a
