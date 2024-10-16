{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Ring where

import Choreography
import Data.Proxy
import GHC.TypeLits (KnownSymbol)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Environment
import Control.Selective
import Debug.Trace
import Choreography.Location

-- an edge of the ring is represented as a tuple of two locaitons l and l' where
-- l is on the left of l'
data Edge = forall l l'.
  (KnownSymbol l, KnownSymbol l') => Edge (Proxy l) (Proxy l')

instance Show Edge where
  show (Edge (e1 :: Proxy l1) (e2 :: Proxy l2)) = toLocTm e1 ++ "->" ++ toLocTm e2


ifBoolTest = ifBool (Pure (V [|| False ||])) (EffG (V [|| print "t" ||])) (EffG (V [|| print "f" ||]))

-- a ring is a sequence of edges
type Ring = [Edge]

type Label = Int

list :: Selective f => f [a] -> f c -> f ((a, [a]) -> c) -> f c
list l k = branch (p <$> l) (const <$> k)
  where
    p [] = Left ()
    p (x:xs) = Right (x, xs)

f = (pure (\l -> pure (\r -> lift (print ("==",l, r)) >> pure (l == r))))

f2 = (pure (\l -> pure (\r -> lift (print ("max", l,r)) >> put (max l r))))

ringLeader :: Ring -> Choreo (StateT Label IO) ()
ringLeader ring = Loop (\k -> unroll ring k)
  where
    unroll :: Ring -> Choreo (StateT Label IO) () -> Choreo (StateT Label IO) ()
    unroll []     k = k
    unroll (x:xs) k = traceShow x $
      ifBool (talkToRight x)
        (Pure (V [|| () ||]))
        (unroll xs k)

    talkToRight :: Edge -> Choreo (StateT Label IO) Bool
    talkToRight (Edge left right) = do
      Let ((left, Eff left (V [|| pure () ||]), V [|| \_ -> get ||]) ~~> right) $
        \labelLeft ->
          Let (right `locally` (V [|| get ||])) $
            \labelRight ->
            Let (LocalAp right (LocalAp right (Eff right (V [|| f ||])) labelLeft) labelRight) $
              \finished -> do

                (condBool right finished \case
                  True  -> Fmap (V [|| \_ -> True ||]) $
                    right `locally` (V[|| (lift $ putStrLn "I'm the leader") ||])
                    **> EffG (V [|| get >>= lift . print ||])

                  False ->
                    Fmap (V [|| \_ -> False ||]) $
                    LocalAp right (LocalAp right (Eff right (V [|| f2 ||])) labelLeft) labelRight)

nodeA :: Proxy "A"
nodeA = Proxy

nodeB :: Proxy "B"
nodeB = Proxy

nodeC :: Proxy "C"
nodeC = Proxy

nodeD :: Proxy "D"
nodeD = Proxy

ring = [ Edge nodeA nodeB
       , Edge nodeB nodeC
       , Edge nodeC nodeD
       , Edge nodeD nodeA
       ]

