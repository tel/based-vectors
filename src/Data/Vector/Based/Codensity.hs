{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

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
-- basis vectors would be to transform them using the @Codensity@
-- monad. This allows vectors to be manipulated by standard monadic
-- and applicative combinators.
--
-- These @Codensity@ transformed vectors can be expensive to examine
-- intermediate values and thus are produced here as a reference
-- implementation.
--
-- In this implementation, we could replace the 'Num' constraint with
-- a suitable ring class and even end up defining left or right
-- modules over a ring. If this is desirable, then a suitable
-- @newtype@ could be used along with a partial 'Num' instance.

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

type Vectable s b = (Num s, Ord s, Ord b)

returnV :: Num s => b -> Map b s
returnV a = Map.singleton a 1

joinV :: (Num s, Ord b) => Map (Map b s) s -> Map b s
joinV = Map.unionsWith (+)
      . map (\(m, v) -> fmap (v *) m)
      . Map.toList

bindV :: Vectable s b' => Map b s -> (b -> Map b' s) -> Map b' s
bindV m f = joinV (Map.mapKeys f m)

aVect :: Vectable s b => Map b s -> Vect s b
aVect x = Vect $ \inj mp -> fmap mp (bindV x inj)

mapVect :: Vectable s b => Vect s b -> Map b s
mapVect v = runVect v returnV id

singleton :: Vectable s b => b -> s -> Vect s b
singleton s m = aVect (Map.singleton s m)

mapField :: (s -> s) -> Vect s b -> Vect s b
mapField f v = Vect $ \k m -> runVect v k (f . m)

at :: Vectable s b => b -> Vect s b -> s
at b v = fromMaybe 0 (Map.lookup b (mapVect v))

newtype Vect s a =
  Vect { runVect :: forall b . Ord b => (a -> Map b s) -> (s -> s) -> Map b s }

instance Vectable s b => Eq (Vect s b) where
  v == v' = mapVect v == mapVect v'

instance (Show s, Show b, Vectable s b) => Show (Vect s b) where
  showsPrec p v = showsPrec p (mapVect v)

instance Vectable s b => Ord (Vect s b) where
  compare v v' = compare (mapVect v) (mapVect v')

instance Functor (Vect s) where
  fmap f v = Vect $ \k m -> runVect v (\a -> k (f a)) m

instance Applicative (Vect s) where
  pure  = return
  (<*>) = ap

instance Monad (Vect s) where
  return a = Vect $ \k m -> k a
  z >>= f  = Vect $ \k m -> runVect z (\a -> runVect (f a) k m) m

instance Vectable s b => AdditiveGroup (Vect s b) where
  zeroV   = aVect Map.empty
  negateV = mapField negate

  -- This is expensive!
  a ^+^ b = aVect (Map.mergeWithKey go id id (mapVect a) (mapVect b)) where
    go _ a b = let r = a + b in if r == 0 then Nothing else Just r

instance Vectable s b => VectorSpace (Vect s b) where
  type Scalar (Vect s b) = s
  s *^ v = mapField (s*) v

instance Vectable s b => HasBasis (Vect s b) where
  type Basis (Vect s b) = b
  basisValue     = flip singleton 1
  decompose      = Map.toList . mapVect
  decompose' v b = fromMaybe 0 (Map.lookup b (mapVect v))

dot :: Vectable s b => Vect s b -> Vect s b -> s
dot a b = at True (liftM2 (==) a b)

class Universe a where
  universe :: [a]

codot :: (Universe b, Vectable s b) => Vect s (b, b)
codot = aVect (Map.fromList (map (\a -> ((a,a), 1)) universe))
