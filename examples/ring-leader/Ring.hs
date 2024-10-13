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

-- an edge of the ring is represented as a tuple of two locaitons l and l' where
-- l is on the left of l'
data Edge = forall l l'.
  (KnownSymbol l, KnownSymbol l') => Edge (Proxy l) (Proxy l')

-- a ring is a sequence of edges
type Ring = [Edge]

type Label = Int

list :: Selective f => f [a] -> f c -> f ((a, [a]) -> c) -> f c
list l k = branch (p <$> l) (const <$> k)
  where
    p [] = Left ()
    p (x:xs) = Right (x, xs)

f = (pure (\l -> pure (\r -> pure (l == r))))

f2 = (pure (\l -> pure (\r -> put (max l r))))

ringLeader :: Ring -> Choreo (StateT Label IO) ()
ringLeader ring = Loop (\k -> unroll ring k)
  where
    unroll :: Ring -> Choreo (StateT Label IO) () -> Choreo (StateT Label IO) ()
    unroll []     k = k
    unroll (x:xs) k = do
      ifBool (talkToRight x)
        (Pure (V [|| () ||]))
        (unroll xs k)

    talkToRight :: Edge -> Choreo (StateT Label IO) Bool
    talkToRight (Edge left right) = do
      let labelLeft = (left, Eff left (V [|| pure () ||]), V [|| \_ -> get ||]) ~~> right
      let labelRight = right `locally` (V [|| get ||])

      let finished =
            LocalAp right (LocalAp right (Eff right (V [|| f ||])) labelLeft) labelRight

{-
      right `locally` \un ->
                        return $ (==) <$> (un <$> labelLeft) <*> (un <$> labelRight)
                        -}

      condBool right finished \case
        True  -> Fmap (V [|| \_ -> True ||]) $
          right `locally` (V[|| (lift $ putStrLn "I'm the leader") ||])
        False ->
          Fmap (V [|| \_ -> False ||]) $
          LocalAp right (LocalAp right (Eff right (V [|| f2 ||])) labelLeft) labelRight
--          right `locally` \un -> put (max (un labelLeft) (un labelRight))

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

