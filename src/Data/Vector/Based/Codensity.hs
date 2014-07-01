{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Data.Vector.Based.Codensity
-- Copyright   :  (c) 2014 Joseph T. Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph T. Abrahamson <me@jspha.com>
-- Stability   :  experimental
-- Portability :  RankNTypes, TypeFamilies
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
-- applicative combinators.
--
-- These Codensity transformed vectors can be expensive to examine
-- intermediate values and thus are produced here as a reference
-- implementation.

module Data.Vector.Based.Codensity (
  Vect
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.AdditiveGroup
import           Data.Basis
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import           Data.VectorSpace

returnV :: a -> Map a Int
returnV a = Map.singleton a 1

joinV :: Ord s => Map (Map s Int) Int -> Map s Int
joinV = Map.unionsWith (+)
      . map (\(m, v) -> fmap (*v) m)
      . Map.toList

bindV :: Ord b => Map a Int -> (a -> Map b Int) -> Map b Int
bindV m f = joinV (Map.mapKeys f m)

aVect :: Ord s => Map s Int -> Vect s
aVect x = Vect $ \inj mp -> fmap mp (bindV x inj)

mapVect :: Ord s => Vect s -> Map s Int
mapVect v = runVect v returnV id

singleton :: Ord s => s -> Int -> Vect s
singleton s m = aVect (Map.singleton s m)

mapField :: (Int -> Int) -> Vect s -> Vect s
mapField f v = Vect $ \k m -> runVect v k (f . m)

newtype Vect a =
  Vect { runVect :: forall s . Ord s => (a -> Map s Int) -> (Int -> Int) -> Map s Int }

instance Ord s => Eq (Vect s) where
  v == v' = mapVect v == mapVect v'

instance (Show s, Ord s) => Show (Vect s) where
  showsPrec p v = showsPrec p (mapVect v)

instance Ord s => Ord (Vect s) where
  compare v v' = compare (mapVect v) (mapVect v')

instance Functor Vect where
  fmap f v = Vect $ \k m -> runVect v (\a -> k (f a)) m

instance Applicative Vect where
  pure  = return
  (<*>) = ap

instance Monad Vect where
  return a = Vect $ \k m -> k a
  z >>= f  = Vect $ \k m -> runVect z (\a -> runVect (f a) k m) m

instance Ord s => AdditiveGroup (Vect s) where
  zeroV   = aVect Map.empty
  negateV = mapField negate

  -- This is expensive!
  a ^+^ b = aVect (Map.mergeWithKey go id id (mapVect a) (mapVect b)) where
    go :: s -> Int -> Int -> Maybe Int
    go _ a b = let r = a + b in if r == 0 then Nothing else Just r

instance Ord s => VectorSpace (Vect s) where
  type Scalar (Vect s) = Int
  s *^ v = mapField (s*) v

instance Ord s => HasBasis (Vect s) where
  type Basis (Vect s) = s
  basisValue     = flip singleton 1
  decompose      = Map.toList . mapVect
  decompose' v b = fromMaybe 0 (Map.lookup b (mapVect v))
