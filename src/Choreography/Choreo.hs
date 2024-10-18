{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo where

import Choreography.Location
import Choreography.Network
import Control.Monad.Freer
import Data.List
import Data.Proxy
import GHC.TypeLits
import Control.Selective hiding (branch)
import qualified Control.Selective as S
import Data.Bifunctor
import Data.Functor
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Typeable
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set

data V a = V { cv :: Code Q a }

data FreeApplicative v f a where
  Pure :: V a -> FreeApplicative v f a

  -- Building computations on nodes
  Eff :: KnownSymbol l => Proxy l -> V (f a) -> FreeApplicative v f (a @ l)
  -- Application on a specific node
  LocalAp ::
    KnownSymbol l => Proxy l -> FreeApplicative v f ((a -> f b) @ l)
                  -> FreeApplicative v f (a @ l) -> FreeApplicative v f (b @ l)

  -- Language to describe a choreography
  EffG :: V (f a) -> FreeApplicative v f a
  Fmap :: V (a -> b) -> FreeApplicative v f a -> FreeApplicative v f b
  Ap :: FreeApplicative v f (a -> b) -> FreeApplicative v f a -> FreeApplicative v f b
  Broadcast :: (Read a, Show a, KnownSymbol l, Typeable a)
            => Proxy l -> FreeApplicative v f (a @ l)
            -> FreeApplicative v f a
  Comm  :: (Show a, Read a, KnownSymbol l, KnownSymbol l', Typeable a) => Proxy l -> FreeApplicative v f (a @ l) -> Proxy l' -> FreeApplicative v f (a @ l')
  Let :: FreeApplicative v f a -> (FreeApplicative v f a -> FreeApplicative v f b) -> FreeApplicative v f b
  Loop :: (FreeApplicative v f a -> FreeApplicative v f a) -> FreeApplicative v f a
  Dragon :: v f a -> FreeApplicative v f a
  SelectN :: FreeApplicative v f (Either a b) -> FreeApplicative v f (a -> b) -> FreeApplicative v f b


toAp :: KnownSymbol l => Proxy l -> V (f a) -> FreeApplicative v f (a @ l)
toAp l x = Eff l x

-- | Monad for writing choreographies.
type Choreo v m = FreeApplicative v m

newtype V1 m a = V1 (Code Q (m a))

staged :: forall m a . (Selective m, Monad m) => Choreo V1 m a-> Code Q (m a)
staged c =
  case c of
    Pure a -> [|| pure $$(cv a) ||]
    Dragon (V1 ma) -> ma
    Eff _ c -> [|| fmap wrap $$(cv c) ||]
    EffG v  -> cv v
    Fmap f a -> [|| fmap $$(cv f) $$(staged a) ||]
    Ap fa a        -> [|| $$(staged fa) <*> $$(staged a) ||]
    Broadcast l e   -> [|| (fmap unwrap) $$(staged e) ||]
    Comm l1 fa l2  -> [|| wrap . unwrap <$> $$(staged fa) ||]
    LocalAp l m a  -> [|| do
                            m' <- unwrap <$> $$(staged m)
                            a' <- $$(staged a)
                            wrap <$> (m' (unwrap a')) ||]


    Loop f         -> [|| let res = $$(staged (f (Dragon (V1 [|| res ||])))) in res ||]
    SelectN fe fab -> [|| select $$(staged fe) $$(staged fab) ||]
    Let a b -> [|| do { l <- $$(staged a); $$(staged (b (Dragon (V1 [|| pure l ||])))) } ||]



newtype R m a = R { runR :: (Code Q (Network m a)) }

genRecv :: forall a m . Typeable a => Code Q (LocTm -> Network m a)
genRecv = unsafeCodeCoerce ([| recv |] `appTypeE` (liftTypeable (typeRep (Proxy @a))))

liftTypeable :: TypeRep -> Q Type
liftTypeable tr
  | tr == typeRep (Proxy @Bool) = [t| Bool |]
  | tr == typeRep (Proxy @Int)  = [t| Int |]
  | tr == typeRep (Proxy @())   = [t| () |]
  | tr == typeRep (Proxy @(Either () ())) = [t| Either () () |]
  | otherwise = error $ "No TR:" ++  (show tr)

stagedEpp :: forall m a . Choreo R m a -> LocTm -> (R m) a
stagedEpp c l' =
  case c of
    Pure a -> R [|| pure ($$(cv a)) ||]
    Dragon ma -> ma
    EffG v -> R $ [|| run $$(cv v) ||]

    Eff loc c
      | toLocTm loc == l' -> R [|| fmap wrap $ run $$(cv c) ||]
      | otherwise -> R [|| pure Empty ||]
    Fmap f a -> R [|| $$(cv f) <$> $$(runR $ stagedEpp a l') ||]
    Ap fa a -> R [|| $$(runR $ stagedEpp fa l') <*> $$(runR $ stagedEpp a l') ||]
    Broadcast l e -> sel l (stagedEpp e l')
    Comm l1 fa l2 -> handlerComm l1 (stagedEpp fa l') l2
    LocalAp l m a -> handlerLoc l (stagedEpp m l') (stagedEpp a l')
    Loop f -> R [|| let res = $$(runR $ stagedEpp (f (Dragon (R [|| res ||]))) l')
                    in res ||]
    Let a b -> R [|| do { l <- $$(runR $ stagedEpp a l'); $$(runR $ stagedEpp (b (Dragon (R [|| pure l ||]))) l') } ||]
    SelectN fe fab -> R [|| select $$(runR $ stagedEpp fe l') $$(runR $ stagedEpp fab l') ||]


  where
    sel :: (Typeable z1, Read z1, Show z1, KnownSymbol l) => Proxy l -> R m (z1 @  l) -> R m z1
    sel l (R s)
      | toLocTm l == l' = R $ [|| do
          v <- $$s
          broadcast (unwrap v)
          return (unwrap v) ||]

      | otherwise = R $ [|| do
          v <- $$s
          $$(genRecv) $$(liftTyped (toLocTm l)) ||]

    handlerComm :: (forall l l' z . (Typeable z, Show z, Read z, KnownSymbol l, KnownSymbol l') => Proxy l -> R m (z @ l) -> Proxy l' -> R m (z @ l'))
    handlerComm s (R a) r
      | toLocTm s == l' = R $ [|| $$a >>= \a' -> send (unwrap a') $$(liftTyped $ (toLocTm r)) >> return Empty ||]
      | toLocTm r == l' = R $ [|| $$a >> (wrap <$> $$(genRecv) $$(liftTyped $ toLocTm s)) ||]
      | otherwise       = R $ [|| Empty <$ $$(a) ||]


    handlerLoc :: (forall l y z  . KnownSymbol l => Proxy l -> R m ((y -> m z) @ l) -> R m (y @ l) -> R m (z @ l))
    handlerLoc l (R p) (R m)
      | toLocTm l == l' = R [|| (unwrap <$> $$p) >>= \p' -> (unwrap <$> $$m) >>= \m' -> (wrap <$> run (p' m')) ||]
      | otherwise       = R [|| Empty <$ ($$p *> $$m) ||]

-- A map showing which other locations are communicated with.
type CommunicationMap = Map.Map LocTm (Set.Set LocTm)

noComms :: CommunicationMap
noComms = Map.empty

comm l1 l2 = Map.singleton l1 (Set.singleton l2)

unionCM :: CommunicationMap -> CommunicationMap -> CommunicationMap
unionCM = Map.unionWith (Set.union)


newtype CM m a = CM (Map.Map LocTm (Set.Set LocTm))


-- | Compute which nodes could communicate with each other.
communicationMap :: forall v m a . (Selective m, Monad m) => Choreo CM m a -> CommunicationMap
communicationMap c =  case c of
    Pure a -> noComms
    Dragon (CM ma) -> ma
    Eff _ c -> noComms
    EffG v  -> noComms
    Fmap f a -> communicationMap a
    Ap fa a        -> communicationMap fa `unionCM` communicationMap a
    Broadcast l e   -> -- TODO
                        communicationMap e
    Comm l1 fa l2  -> communicationMap fa `unionCM` comm (toLocTm l1) (toLocTm l2)
    LocalAp l m a  -> communicationMap m `unionCM` communicationMap a

    Loop f         -> communicationMap (f (Dragon (CM noComms)))
    SelectN fe fab -> communicationMap fe `unionCM` communicationMap fab
    Let a b -> communicationMap a `unionCM` communicationMap (b (Dragon (CM noComms)))


-- * Choreo operations

-- | Perform a local computation at a given location.
locally :: KnownSymbol l
        => Proxy l -- ^ Location performing the local computation.
        -> V (m a) -- ^ The local computation
        -> Choreo v m (a @ l)
locally l m = Eff l m

-- | Communication between a sender and a receiver.
(~>) :: (Typeable a, Show a, Read a, KnownSymbol l, KnownSymbol l')
     => (Proxy l
        , Choreo v m (a @ l))  -- ^ A pair of a sender's location and a value located
                          -- at the sender
     -> Proxy l'          -- ^ A receiver's location.
     -> Choreo v m (a @ l')
(~>) (l, a) l' = Comm l a l'


-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Typeable a, Applicative m, Show a, Read a, KnownSymbol l, KnownSymbol l')
      => (Proxy l, Choreo v m (b @ l), V (b -> m a)) -- ^ A pair of a sender's location and a local
                                    -- computation.
      -> Proxy l'                   -- ^ A receiver's location.
      -> Choreo v m (a @ l')
(~~>) (l, c, m) l' = do
  let x = LocalAp l (Eff l (V  [|| pure $$(cv m) ||])) c
  (l, x) ~> l'

(**>) :: FreeApplicative v m b -> FreeApplicative v m a -> FreeApplicative v m b
(**>) a b = Ap (Fmap (V [|| \ b _ -> b ||]) a) b

e :: V ((a -> c) -> (b -> c) -> Either a b -> c)
e = V [|| either ||]

branchL :: (Typeable a, Typeable b, Show a, Show b, Read a, Read b, KnownSymbol l) => Proxy l -> Choreo v m ((Either a b) @ l) -> Choreo v m (a -> c) -> Choreo v m (b -> c) -> Choreo v m c
branchL p x l r = branch (Broadcast p x) l r


cb = mapLoc  (bool (Left ()) (Right ()))
  where
    bool t f True = t
    bool t f False = f

branch :: Choreo v m ((Either a b)) -> Choreo v m (a -> c) -> Choreo v m (b -> c) -> Choreo v m c
branch x l r = SelectN (SelectN g q) r
  where
    g = Fmap (V [|| bimap id Left ||])  x

    q = Fmap (V [|| fmap Right ||]) l

bool t f True = t
bool t f False = f

ifBool :: Choreo v m Bool -> Choreo v m a -> Choreo v m a -> Choreo v m a
ifBool b t f = branch (Fmap (V [|| bool (Left ()) (Right ()) ||]) b) (Fmap (V [|| const ||]) t) (Fmap (V [|| const ||]) f)


condBool :: (KnownSymbol l) => Proxy l -> Choreo v m (Bool @ l) -> (Bool -> Choreo v  m b) -> Choreo v m b
condBool l b k = ifBool (Broadcast l b) (k True) (k False)


