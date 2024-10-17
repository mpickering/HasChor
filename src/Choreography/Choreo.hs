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

data V a = V { cv :: Code Q a }

data Var m a = Var (Code Q (Network m a)) | Var2 (Code Q (m a))

data FreeApplicative f a where
  Pure :: V a -> FreeApplicative f a
  Eff :: KnownSymbol l => Proxy l -> V (f a) -> FreeApplicative f (a @ l)
  EffG :: V (f a) -> FreeApplicative f a
  Dragon :: Var f a -> FreeApplicative f a
  Fmap :: V (a -> b) -> FreeApplicative f a -> FreeApplicative f b
  Ap :: FreeApplicative f (a -> b) -> FreeApplicative f a -> FreeApplicative f b
  Broadcast :: (Read a, Show a, KnownSymbol l, Typeable a)
            => Proxy l -> FreeApplicative f (a @ l)
            -> FreeApplicative f a
  {-
  Cond  :: (Read a, Show a, KnownSymbol l, Typeable a) => Proxy l
        -> FreeApplicative f (a @ l)
        -> FreeApplicative f (a -> r)
        -> FreeApplicative f r
        -}
  Comm  :: (Show a, Read a, KnownSymbol l, KnownSymbol l', Typeable a) => Proxy l -> FreeApplicative f (a @ l) -> Proxy l' -> FreeApplicative f (a @ l')
  Let :: FreeApplicative f a -> (FreeApplicative f a -> FreeApplicative f b) -> FreeApplicative f b
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



toAp :: KnownSymbol l => Proxy l -> V (f a) -> FreeApplicative f (a @ l)
toAp l x = Eff l x

-- * The Choreo monad

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
    EffG v  -> cv v
    Fmap f a -> [|| fmap $$(cv f) $$(staged a) ||]
    Ap fa a        -> [|| $$(staged fa) <*> $$(staged a) ||]
    Broadcast l e   -> [|| (fmap unwrap) $$(staged e) ||]
--    Cond _ a f       -> [|| (unwrap <$>) $$(staged a) <**> $$(staged f) ||]
    Comm l1 fa l2  -> [|| wrap . unwrap <$> $$(staged fa) ||]
    LocalAp l m a  -> [|| do
                            m' <- unwrap <$> $$(staged m)
                            a' <- $$(staged a)
                            wrap <$> (m' (unwrap a')) ||]


    Loop f         -> [|| let res = $$(staged (f (Dragon (Var2 [|| res ||])))) in res ||]
    SelectN fe fab -> [|| select $$(staged fe) $$(staged fab) ||]
    Let a b -> [|| do { l <- $$(staged a); $$(staged (b (Dragon (Var2 [|| pure l ||])))) } ||]



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

stagedEpp :: forall m a . Choreo m a -> LocTm -> (R m) a
stagedEpp c l' =
  case c of
    Pure a -> R [|| pure ($$(cv a)) ||]
    Dragon (Var ma) -> R $ ma
    EffG v -> R $ [|| run $$(cv v) ||]

    Eff loc c
      | toLocTm loc == l' -> R [|| fmap wrap $ run $$(cv c) ||]
      | otherwise -> R [|| pure Empty ||]
    Fmap f a -> R [|| $$(cv f) <$> $$(runR $ stagedEpp a l') ||]
    Ap fa a -> R [|| $$(runR $ stagedEpp fa l') <*> $$(runR $ stagedEpp a l') ||]
    Broadcast l e -> sel l (stagedEpp e l')
    Comm l1 fa l2 -> handlerComm l1 (stagedEpp fa l') l2
    LocalAp l m a -> handlerLoc l (stagedEpp m l') (stagedEpp a l')
    Loop f -> R [|| let res = $$(runR $ stagedEpp (f (Dragon (Var [|| res ||]))) l')
                    in res ||]
    Let a b -> R [|| do { l <- $$(runR $ stagedEpp a l'); $$(runR $ stagedEpp (b (Dragon (Var [|| pure l ||]))) l') } ||]
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

-- * Choreo operations

-- | Perform a local computation at a given location.
locally :: KnownSymbol l
        => Proxy l           -- ^ Location performing the local computation.
        -> V (m a) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> Choreo m (a @ l)
locally l m = Eff l m

-- | Communication between a sender and a receiver.
(~>) :: (Typeable a, Show a, Read a, KnownSymbol l, KnownSymbol l')
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
(~~>) :: (Typeable a, Applicative m, Show a, Read a, KnownSymbol l, KnownSymbol l')
      => (Proxy l, Choreo m (b @ l), V (b -> m a)) -- ^ A pair of a sender's location and a local
                                    -- computation.
      -> Proxy l'                   -- ^ A receiver's location.
      -> Choreo m (a @ l')
(~~>) (l, c, m) l' = do
  let x = LocalAp l (Eff l (V  [|| pure $$(cv m) ||])) c
  (l, x) ~> l'

(**>) :: FreeApplicative m b -> FreeApplicative m a -> FreeApplicative m b
(**>) a b = Ap (Fmap (V [|| \ b _ -> b ||]) a) b

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
e :: V ((a -> c) -> (b -> c) -> Either a b -> c)
e = V [|| either ||]

branch :: (Typeable a, Typeable b, Show a, Show b, Read a, Read b, KnownSymbol l) => Proxy l -> Choreo m ((Either a b) @ l) -> Choreo m (a -> c) -> Choreo m (b -> c) -> Choreo m c
branch p x l r = branch2 (Broadcast p x) l r



cb = mapLoc  (bool (Left ()) (Right ()))
  where
    bool t f True = t
    bool t f False = f

branch2 :: Choreo m ((Either a b)) -> Choreo m (a -> c) -> Choreo m (b -> c) -> Choreo m c
branch2 x l r = SelectN (SelectN g q) r
  where
    g = Fmap (V [|| bimap id Left ||])  x

    q = Fmap (V [|| fmap Right ||]) l


bool t f True = t
bool t f False = f

ifBool :: Choreo m Bool -> Choreo m a -> Choreo m a -> Choreo m a
ifBool b t f = branch2 (Fmap (V [|| bool (Left ()) (Right ()) ||]) b) (Fmap (V [|| const ||]) t) (Fmap (V [|| const ||]) f)


condBool :: (KnownSymbol l) => Proxy l -> Choreo m (Bool @ l) -> (Bool -> Choreo m b) -> Choreo m b
condBool l b k = branch l (Fmap (V [|| cb ||])  b) (Fmap (V [|| const ||]) (k True)) (Fmap (V [|| const ||]) (k False))
  --Cond l b (Pure (V [|| bool ||]) `Ap` k False `Ap` k True)

{-
condBool' :: (KnownSymbol l) => Proxy l -> Choreo m (Bool @ l) -> (Bool -> Choreo m b) -> Choreo m b
condBool' l b k =
  Select l _ _
  -}

--condUnit :: (KnownSymbol l) => Proxy l -> Choreo m (() @ l) -> (() -> Choreo m b) -> Choreo m b
--condUnit l b k =
--  Select l (Fmap (V Left [|| Left ||]) b) (Fmap (V (\b _ -> b) [|| (\b _ -> b) ||]) (k ()))

