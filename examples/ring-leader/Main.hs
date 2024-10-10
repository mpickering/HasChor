{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE ApplicativeDo  #-}

module Main where

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

ringLeader :: Ring -> Choreo (StateT Label IO) ()
ringLeader ring = Loop (\k -> unroll ring k)
  where
    unroll :: Ring -> Choreo (StateT Label IO) () -> Choreo (StateT Label IO) ()
    unroll []     k = k
    unroll (x:xs) k = do
      ifS (talkToRight x)
        (pure ())
        (unroll xs k)

    talkToRight :: Edge -> Choreo (StateT Label IO) Bool
    talkToRight (Edge left right) = do
      let labelLeft = (left, Eff left (pure ()), \_ -> get) ~~> right
      let labelRight = right `locally` get

      let finished =
            LocalAp right (LocalAp right (Eff right (pure (\l -> pure (\r -> pure (l == r))))) labelLeft) labelRight

{-
      right `locally` \un ->
                        return $ (==) <$> (un <$> labelLeft) <*> (un <$> labelRight)
                        -}

      condBool right finished \case
        True  -> do
          right `locally` (lift $ putStrLn "I'm the leader")
          return True
        False -> do
          LocalAp right (LocalAp right (Eff right (pure (\l -> pure (\r -> put (max l r))))) labelLeft) labelRight
--          right `locally` \un -> put (max (un labelLeft) (un labelRight))
          return False

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

main :: IO ()
main = do
  [loc] <- getArgs
  putStrLn "Please input a label:"
  label <- read <$> getLine
  runStateT (runChoreography config (ringLeader ring) loc) label
  return ()
  where
    config = mkHttpConfig [ ("A", ("localhost", 4242))
                          , ("B", ("localhost", 4343))
                          , ("C", ("localhost", 4444))
                          , ("D", ("localhost", 4545))
                          ]
