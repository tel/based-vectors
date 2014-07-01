
-- |
-- Module      :  Data.Vector.Based
-- Copyright   :  (c) 2014 Joseph T. Abrahamson
-- License     :  BSD3
-- Maintainer  :  Joseph T. Abrahamson <me@jspha.com>
-- Stability   :  experimental
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


