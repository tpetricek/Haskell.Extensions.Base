{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Or
-- Copyright   :  (c) Tomas Petricek 2011,
--                (c) University of Cambridge 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Monadic choice with left-bias. Based on the MonadPlus reform proposal:
-- http://www.haskell.org/haskellwiki/MonadPlus_reform_proposal
-- and used by 'docase' notation desugaring.
--
-----------------------------------------------------------------------------

module Control.Monad.Or (
      MonadOr (morelse, morzero)
  ) where

import Prelude
import Control.Monad (liftM)

-- | `MonadOr` type class. Minimal definition: `morelse`
--
-- Instances should satisfy the laws:
--
-- * Naturality :
--
--   > morelse (liftM f ma) (liftM f mb) = liftM f (morelse ma mb)
--
-- * Associativity :
--
--   > morelse ma (morelse mb mc) = morelse (morelse ma mb) mc
--
-- * Unit :
--
--   > morelse ma mzero = ma = morelse mzero ma
--
-- * Left-bias :
--
--   > morelse ma (liftM f ma) = ma
--
class Monad m => MonadOr m where

    -- | zero element
    morzero :: m a 

    -- | an associative operation
    morelse :: m a -> m a -> m a


instance MonadOr [] where
    morelse (x:xs) (y:ys) = x:(morelse xs ys)
    morelse [] ys = ys
    morelse xs [] = xs

    morzero = fail "!"

instance MonadOr Maybe where
    morelse (Just a) _ = Just a
    morelse _ mb = mb
    
    morzero = fail "!"