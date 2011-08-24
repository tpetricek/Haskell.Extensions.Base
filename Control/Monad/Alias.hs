{-# LANGUAGE Safe, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Alias
-- Copyright   :  (c) Tomas Petricek 2011,
--                (c) University of Cambridge 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Monadic aliasing. Used by the desugaring of 'docase' notation.
--
-----------------------------------------------------------------------------

module Control.Monad.Alias where

import Prelude
import Control.Monad (liftM)

-- | `MonadAlias` type class. 
--
-- Instances should satisfy the laws:
--
-- * TODO
--
class Monad m => MonadAlias m where

    malias :: m a -> m (m a)
    malias = return

instance Monad m => MonadAlias m