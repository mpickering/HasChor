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
import Data.Bifunctor
import Data.Functor
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data V a = V { v :: a, cv :: Code Q a }

data Var m a = Var (Code Q (Network m a)) | Var2 (Code Q (m a))

data FreeApplicative f a where
  Pure :: V a -> FreeApplicative f a
  Eff :: KnownSymbol l => Proxy l -> V (f a) -> FreeApplicative f (a @ l)
  Dragon :: Var f a -> FreeApplicative f a
  Fmap :: V (a -> b) -> FreeApplicative f a -> FreeApplicative f b
  Ap :: FreeApplicative f (a -> b) -> FreeApplicative f a -> FreeApplicative f b
  Select :: (Read a, Show a, KnownSymbol l) => Proxy l -> FreeApplicative f (Either (a @ l) b) -> FreeApplicative f (a -> b) -> FreeApplicative f b
  Comm  :: (Show a, Read a, KnownSymbol l, KnownSymbol l') => Proxy l -> FreeApplicative f (a @ l) -> Proxy l' -> FreeApplicative f (a @ l')
  -- Application on a specific node
  LocalAp ::
    KnownSymbol l => Proxy l -> FreeApplicative f ((a -> f b) @ l)
                  -> FreeApplicative f (a @ l) -> FreeApplicative f (b @ l)
  Loop :: (FreeApplicative f a -> FreeApplicative f a) -> FreeApplicative f a
  SelectN :: FreeApplicative f (Either a b) -> FreeApplicative f (a -> b) -> FreeApplicative f b

{-
instance Functor (FreeApplicative f) where
  fmap f x = Fmap f x

instance Applicative (FreeApplicative f) where
  pure = Pure
  x <*> y = Ap x y
  -}



interpAp :: (Selective m, Applicative m) => (forall l z b . (Show z, Read z, KnownSymbol l) => Proxy l -> m (Either (z @ l) b) -> m (z -> b) -> m b)
                            -> (forall l l' z . (Show z, Read z, KnownSymbol l, KnownSymbol l') => Proxy l -> m (z @ l) -> Proxy l' -> m (z @ l'))
                            -> (forall y l z  . KnownSymbol l => Proxy l -> m ((y -> f z) @ l) -> m (y @ l) -> m (z @ l))
                            -> (forall l a . KnownSymbol l => Proxy l -> f a -> m (a @ l))
                            -> FreeApplicative f a -> m a
interpAp _ _ _ k (Pure a) = pure (v a)
interpAp _ _ _ k (Eff l x)  = k l (v x)
interpAp sel comm loc k (Fmap f x) = v f <$> interpAp sel comm loc k x
interpAp sel comm loc k (Ap f x) = interpAp sel comm loc k f <*> interpAp sel comm loc k x
interpAp sel comm loc k (Select l scrut cont) = sel l (interpAp sel comm loc k scrut) (interpAp sel comm loc k cont)
interpAp sel comm loc k (Comm l1 c l2) = comm l1 (interpAp sel comm loc k c) l2
interpAp sel comm loc k (LocalAp l ff fa) =
  loc l (interpAp sel comm loc k ff) (interpAp sel comm loc k fa)
interpAp sel comm loc k (Loop f) = error "todo"
--  let res = f res
--  in interpAp sel comm loc k res
interpAp sel comm loc k (SelectN a b) = select (interpAp sel comm loc k a) (interpAp sel comm loc k b)

toAp :: KnownSymbol l => Proxy l -> V (f a) -> FreeApplicative f (a @ l)
toAp l x = Eff l x

-- * The Choreo monad

-- | A constrained version of `unwrap` that only unwraps values located at a
-- specific location.
type Unwrap l = forall a. a @ l -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents
-- local computations.
{-
data ChoreoSig m a where
  Local :: (KnownSymbol l)
        => Proxy l
        -> (Unwrap l -> m a)
        -> ChoreoSig m (a @ l)

  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
       => Proxy l
       -> a @ l
       -> Proxy l'
       -> ChoreoSig m (a @ l')
       -}

{-
  Cond :: (Show a, Read a, KnownSymbol l)
       => Proxy l
       -> a @ l
       -> (a -> Choreo m b)
       -> ChoreoSig m b
       -}

{-
Loop (\x -> foo >> x)
=>
let res = foo >> res
in res
-}

-- | Monad for writing choreographies.
type Choreo m = FreeApplicative m

staged :: forall m a . (Selective m, Monad m) => Choreo m a-> Code Q (m a)
staged c =
  case c of
    Pure a -> [|| pure $$(cv a) ||]
    Dragon (Var2 ma) -> ma
    Eff _ c -> [|| fmap wrap $$(cv c) ||]
    Fmap f a -> [|| fmap $$(cv f) $$(staged a) ||]
    Ap fa a        -> [|| $$(staged fa) <*> $$(staged a) ||]
    Select l e k   -> [|| select (fmap (bimap unwrap id) $$(staged e)) $$(staged k) ||]
    Comm l1 fa l2  -> [|| wrap . unwrap <$> $$(staged fa) ||]
    LocalAp l m a  -> [|| do
                            m' <- unwrap <$> $$(staged m)
                            a' <- $$(staged a)
                            wrap <$> (m' (unwrap a')) ||]


    Loop f         -> [|| let res = $$(staged (f (Dragon (Var2 [|| res ||])))) in res ||]
    SelectN fe fab -> [|| select $$(staged fe) $$(staged fab) ||]



newtype R m a = R { runR :: (Code Q (Network m a)) }

stagedEpp :: forall m a . Choreo m a -> LocTm -> (R m) a
stagedEpp c l' =
  case c of
    Pure a -> R [|| pure ($$(cv a)) ||]
    Dragon (Var ma) -> R $ ma

    Eff loc c
      | toLocTm loc == l' -> R [|| fmap wrap $ run $$(cv c) ||]
      | otherwise -> R [|| pure Empty ||]
    Fmap f a -> R [|| $$(cv f) <$> $$(runR $ stagedEpp a l') ||]
    Ap fa a -> R [|| $$(runR $ stagedEpp fa l') <*> $$(runR $ stagedEpp a l') ||]
    Select l e k -> sel l (stagedEpp e l') (stagedEpp k l')
    Comm l1 fa l2 -> handlerComm l1 (stagedEpp fa l') l2
    LocalAp l m a -> handlerLoc l (stagedEpp m l') (stagedEpp a l')
    Loop f -> R [|| let res = $$(runR $ stagedEpp (f (Dragon (Var [|| res ||]))) l')
                    in res ||]
    SelectN fe fab -> R [|| select $$(runR $ stagedEpp fe l') $$(runR $ stagedEpp fab l') ||]
{-
  Pure :: a -> FreeApplicative f a
  Eff :: KnownSymbol l => Proxy l -> f a -> FreeApplicative f (a @ l)
  Fmap :: (a -> b) -> FreeApplicative f a -> FreeApplicative f b
  Ap :: FreeApplicative f (a -> b) -> FreeApplicative f a -> FreeApplicative f b
  Select :: (Read a, Show a, KnownSymbol l) => Proxy l -> FreeApplicative f (Either (a @ l) b) -> FreeApplicative f (a -> b) -> FreeApplicative f b
  Comm  :: (Show a, Read a, KnownSymbol l, KnownSymbol l') => Proxy l -> FreeApplicative f (a @ l) -> Proxy l' -> FreeApplicative f (a @ l')
  -- Application on a specific node
  LocalAp ::
    KnownSymbol l => Proxy l -> FreeApplicative f ((a -> f b) @ l)
                  -> FreeApplicative f (a @ l) -> FreeApplicative f (b @ l)
  Loop :: (FreeApplicative f a -> FreeApplicative f a) -> FreeApplicative f a
  SelectN :: FreeApplicative f (Either a b) -> FreeApplicative f (a -> b) -> FreeApplicative f b
  -}



  where
    sel :: (Read z, Show z, KnownSymbol l) => Proxy l -> R m (Either (z @ l) b) -> R m (z -> b) -> R m b
    sel l (R s) (R c)
      | toLocTm l == l' = R $ [|| do
          v <- $$s
          case v of
            Left a -> do
              broadcast (unwrap a)
              k <- $$c
              return $ (k (unwrap a))
            Right no_match -> return no_match ||]

      --broadcast (unwrap a) >> epp (c (unwrap a)) l'
      | otherwise = R $ [|| do
          v <- $$s
          case v of
            Left a -> do
              x <- recv $$(liftTyped (toLocTm l))
              k <- $$c
              return (k x)
            Right b -> return b ||]

    handlerComm :: (forall l l' z . (Show z, Read z, KnownSymbol l, KnownSymbol l') => Proxy l -> R m (z @ l) -> Proxy l' -> R m (z @ l'))
    handlerComm s (R a) r
      | toLocTm s == l' = R $ [|| $$a >>= \a' -> send (unwrap a') $$(liftTyped $ (toLocTm r)) >> return Empty ||]
      | toLocTm r == l' = R $ [|| $$a >> (wrap <$> recv $$(liftTyped $ toLocTm s)) ||]
      | otherwise       = R $ [|| Empty <$ $$(a) ||]


    handlerLoc :: (forall l y z  . KnownSymbol l => Proxy l -> R m ((y -> m z) @ l) -> R m (y @ l) -> R m (z @ l))
    handlerLoc l (R p) (R m)
      | toLocTm l == l' = R [|| (unwrap <$> $$p) >>= \p' -> (unwrap <$> $$m) >>= \m' -> (wrap <$> run (p' m')) ||]
      | otherwise       = R [|| Empty <$ ($$p *> $$m) ||]

-- | Run a `Choreo` monad directly.
runChoreo :: forall m a . (Selective m , Monad m) => Choreo m a -> m a
runChoreo = interpAp (\_ s x -> select (fmap (bimap unwrap id) s) x) handlerComm handlerLoc (\_ -> fmap wrap)
  where
    handlerLoc :: (forall l z  . KnownSymbol l => Proxy l -> m ((y -> m z) @ l) -> m (y @ l) -> m (z @ l))
    handlerLoc _ p m  = (unwrap <$> p) >>= \p' -> (unwrap <$> m) >>= \m' -> (wrap <$> p' m')
    handlerComm :: (forall l l' z . (Show z, Read z, KnownSymbol l, KnownSymbol l') => Proxy l -> m (z @ l) -> Proxy l' -> m (z @ l'))
    handlerComm _ a _ = (wrap . unwrap) <$> a

-- | Endpoint projection.
epp :: forall m a . Choreo m a -> LocTm -> Network m a
epp c l' = interpAp sel handlerComm handlerLoc (\_ -> fmap wrap . run) c
  where
    handlerLoc :: (forall l y z  . KnownSymbol l => Proxy l -> Network m ((y -> m z) @ l) -> Network m (y @ l) -> Network m (z @ l))
    handlerLoc l p m
      | toLocTm l == l' = (unwrap <$> p) >>= \p' -> (unwrap <$> m) >>= \m' -> (wrap <$> run (p' m'))
      | otherwise       = Empty <$ (p *> m)
    handlerComm :: (forall l l' z . (Show z, Read z, KnownSymbol l, KnownSymbol l') => Proxy l -> Network m (z @ l) -> Proxy l' -> Network m (z @ l'))
    handlerComm s a r
      | toLocTm s == l' = a >>= \a' -> send (unwrap a') (toLocTm r) >> return Empty
      | toLocTm r == l' = a >> (wrap <$> recv (toLocTm s))
      | otherwise       = Empty <$ a

    sel :: (Read z, Show z, KnownSymbol l) => Proxy l -> Network m (Either (z @ l) b) -> Network m (z -> b) -> Network m b
    sel l s c
      | toLocTm l == l' = do
          v <- s
          case v of
            Left a -> do
              broadcast (unwrap a)
              k <- c
              return $ (k (unwrap a))
            Right no_match -> return no_match

      --broadcast (unwrap a) >> epp (c (unwrap a)) l'
      | otherwise = do
          v <- s
          case v of
            Left a -> do
              x <- recv (toLocTm l)
              k <- c
              return (k x)
            Right b -> return b

      {-
    handler (Cond l a c)
      | toLocTm l == l' = broadcast (unwrap a) >> epp (c (unwrap a)) l'
      | otherwise       = recv (toLocTm l) >>= \x -> epp (c x) l'
      -}

-- * Choreo operations

-- | Perform a local computation at a given location.
locally :: KnownSymbol l
        => Proxy l           -- ^ Location performing the local computation.
        -> V (m a) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> Choreo m (a @ l)
locally l m = Eff l m

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
     => (Proxy l
        , Choreo m (a @ l))  -- ^ A pair of a sender's location and a value located
                          -- at the sender
     -> Proxy l'          -- ^ A receiver's location.
     -> Choreo m (a @ l')
(~>) (l, a) l' = Comm l a l'


{-
-- | Conditionally execute choreographies based on a located value.
cond :: ((Bounded a, Enum a, Eq a, Show a, Read a, KnownSymbol l))
     => Choreo m (a @ l)  -- ^ A pair of a location and a scrutinee located on
                          -- it.
     -> (a -> Choreo m b) -- ^ A function that describes the follow-up
                          -- choreographies based on the value of scrutinee.
     -> Choreo m b
cond v k = bindS v k

-- | A list of values, equipped with a fast membership test.
data Cases a = Cases [a] (a -> Bool)

-- | The list of all possible values of an enumerable data type.
casesEnum :: (Bounded a, Enum a) => Cases a
casesEnum = Cases [minBound..maxBound] (const True)
-}


{-
-- | Eliminate all specified values @a@ from @f (Either a b)@ by replacing each
-- of them with a given @f a@.
matchS :: (Eq a) => Cases a -> FreeApplicative f a -> (a -> f b) -> f (Either a b)
matchS (Cases cs _) x f = foldr (\c -> eliminate c (f c)) (Left <$> x) cs

-- | Eliminate a specified value @a@ from @f (Either a b)@ by replacing it
-- with a given @f b@.
eliminate :: (Eq a) => a -> FreeApplicative f b -> FreeApplicative f (Either (a @ l) b) -> FreeApplicativ)Either a b)
eliminate x fb fa = select (match x <$> fa) (const . Right <$> fb)
  where
    match _ (Right y) = Right (Right y)
    match x (Left  y) = if x == y then Left () else Right (Left y)
    -}



-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Applicative m, Show a, Read a, KnownSymbol l, KnownSymbol l')
      => (Proxy l, Choreo m (b @ l), V (b -> m a)) -- ^ A pair of a sender's location and a local
                                    -- computation.
      -> Proxy l'                   -- ^ A receiver's location.
      -> Choreo m (a @ l')
(~~>) (l, c, m) l' = do
  let x = LocalAp l (Eff l (V (pure (v m)) [|| pure $$(cv m) ||])) c
  (l, x) ~> l'

(**>) :: FreeApplicative m b -> FreeApplicative m a -> FreeApplicative m b
(**>) a b = Ap (Fmap (V (\b _ -> b) [|| \ b _ -> b ||]) a) b

{-
-- | A variant of `cond` that conditonally executes choregraphies based on the
-- result of a local computation.
cond' :: (Show a, Read a, KnownSymbol l)
      => (Proxy l, Unwrap l -> m a) -- ^ A pair of a location and a local
                                    -- computation.
      -> (a -> Choreo m b)          -- ^ A function that describes the follow-up
                                    -- choreographies based on the result of the
                                    -- local computation.
      -> Choreo m b
cond' (l, m) c = do
  x <- l `locally` m
  cond (l, x) c

k x -> case x of
          True -> k True
          False -> k False
  -}

{-
condBool :: (KnownSymbol l) => Proxy l -> Choreo m (Bool @ l) -> (Bool -> Choreo m b) -> Choreo m b
condBool l b k =
  Select l (m1 . unwrap <$> b) ((\k1 k2 -> (\e -> either (const k1) (const k2) e)) <$> k True <*> k False)
  where

    m1 True = Left (wrap $ Left ())
    m1 False = Left (wrap $ Right ())
--    m1 False = Right (Left False)
--    -}
--
c = (bimap wrap (Left . wrap) . unwrap)
branch :: (Show a, Show b, Read a, Read b, KnownSymbol l) => Proxy l -> Choreo m ((Either a b) @ l) -> Choreo m (a -> c) -> Choreo m (b -> c) -> Choreo m c
branch p x l r = Select p (Select p g q) r
  where
    g = Fmap (V c [|| c ||])  x

    q = Fmap (V (fmap Right) [|| fmap Right ||]) l

    m1 True = Left (wrap ())
    m1 False = Right (Left ())

    m2 = Right


cb = wrap . bool (Left ()) (Right ()) . unwrap
  where
    bool t f True = t
    bool t f False = f

condBool :: (KnownSymbol l) => Proxy l -> Choreo m (Bool @ l) -> (Bool -> Choreo m b) -> Choreo m b
condBool l b k = branch l (Fmap (V cb [|| cb ||])  b) (Fmap (V const [|| const ||]) (k True)) (Fmap (V const [|| const ||]) (k False))

{-
condBool' :: (KnownSymbol l) => Proxy l -> Choreo m (Bool @ l) -> (Bool -> Choreo m b) -> Choreo m b
condBool' l b k =
  Select l _ _
  -}

condUnit :: (KnownSymbol l) => Proxy l -> Choreo m (() @ l) -> (() -> Choreo m b) -> Choreo m b
condUnit l b k =
  Select l (Fmap (V Left [|| Left ||]) b) (Fmap (V (\b _ -> b) [|| (\b _ -> b) ||]) (k ()))

