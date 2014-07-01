{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- |
-- Module      :  Data.Vector.Based
-- Copyright   :  (c) 2014 Joseph T. Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph T. Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  GADTs
--
-- A vector space is represented by a set of linearly independent
-- basis vectors. Normally, if you treat a list or array as a vector
-- then this basis is the index into the array.
--
-- Vector types where the basis elements are polymorphic form a
-- near-monad as explored by Dan Piponi. This types have an
-- interesting structure, but are rarely used since they are either
-- inconvenient or inefficient to work with.
--
-- * http://blog.sigfpe.com/2007/02/monads-for-vector-spaces-probability.html
-- * http://blog.sigfpe.com/2007/03/monads-vector-spaces-and-quantum.html
-- * http://vimeo.com/6590617
--
-- The standard mechanism to make convenient, efficient arbitrary
-- basis vectors would be to transform them using the Codensity monad.
-- This allows vectors to be manipulated by standard monadic and
-- applicative combinators. Unfortunately, this becomes expensive when
-- intermediate values of vectors during computations are queried.
--
-- The mechanism of van der Ploeg and Kiselyov (2014) allows us to
-- have an arbitrary based vector type which is fast to bind and fast
-- to inspect.
--
-- * http://homepages.cwi.nl/~ploeg/papers/zseq.pdf

module Data.Vector.Based where

import           Control.Applicative
import           Control.Monad
import           Data.Map            (Map)
import qualified Data.Map            as Map

newtype Vect_ s b = Vect_ { unVect_ :: Map b s }

returnV :: Num s => b -> Map b s
returnV a = Map.singleton a 1

joinV :: (Num s, Ord b) => Map (Map b s) s -> Map b s
joinV = Map.unionsWith (+)
      . map (\(m, v) -> fmap (v *) m)
      . Map.toList

(>>>)
  :: (Ord b, Ord s, Num s) =>
     (t -> Map k1 s) -> (k1 -> Map b s) -> t -> Map b s
f >>> g = \a -> f a >>- g

(>>-)
  :: (Ord s, Ord b, Num s) => Map k1 s -> (k1 -> Map b s) -> Map b s
m >>- f = (joinV (Map.mapKeys f m))

data K s a b = K { appK :: a -> Map b s }

data Vect s b where
  Vect  :: Map x s -> TList (K s) x b -> Vect s b
  -- This basically ruins it
  Vect1 :: Vect s b -> (b -> Vect s c) -> Vect s c

concatTs :: (TSequence seq, Num s, Ord s, Ord b)
         => seq (K s) a b -> K s a b
concatTs x = case tviewl x of
  TEmptyL -> K returnV
  k :< ks -> K (appK k >>> appK (concatTs ks))

inj :: Map b s -> Vect s b
inj m = Vect m Nil

prj :: (Num s, Ord s, Ord b) => Vect s b -> Map b s
prj (Vect m0 x) = m0 >>- appK (concatTs x)

instance Num s => Functor (Vect s) where
  fmap = liftM

instance Num s => Applicative (Vect s) where
  pure  = return
  (<*>) = ap

instance Num s => Monad (Vect s) where
  return  = inj . returnV
  v >>= f = Vect1 v f

-- Map x s -> TList (K s) x a -> (a -> Vect s b) -> Vect s b

--------------------------------------------------------------------------------

data TViewl s c x y where
  TEmptyL :: TViewl s c x x
  (:<)    :: c x y -> s c y z -> TViewl s c x z

class TSequence s where
  tempty     :: s c x x
  tsingleton :: c x y -> s c x y
  (><)       :: s c x y -> s c y z -> s c x z
  tviewl     :: s c x y -> TViewl s c x y

-- | Not the most efficient, but it'll do for initial implementation.
-- Later this can be swapped out.
infixr 5 :-
data TList c x y where
  Nil  :: TList c x x
  (:-) :: c x y -> TList c y z -> TList c x z

instance TSequence TList where
  tempty = Nil
  tsingleton cxy = cxy :- Nil
  Nil      >< t  = t
  (t :- c) >< t' = t :- (c >< t')
  tviewl Nil      = TEmptyL
  tviewl (t :- c) = t :< c

