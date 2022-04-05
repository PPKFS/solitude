{- |
Copyright: (c) 2022 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

See README for more info
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Solitude ( 
module Relude
, module Optics
, module Optics.State.Operators
, module Relude.Extra.Bifunctor
, module Relude.Extra.Tuple
, module Formatting
, isPrefixOf'
, caseM
, wrap
, composel
, isSuffixOf'
, surroundM
, (<$$>)
, prettyPrintList
, bothAnd
, Text(..)
, Reversing(..)
, reversed
, universeSans
, (%!)
, (<<+~)
, (<<-~)
, MonadRS) where

import Relude
import Optics hiding (uncons)
import Optics.State.Operators
import qualified Data.Text as T
import Relude.Extra.Bifunctor
import Relude.Extra.Tuple
import qualified Data.Text
import Formatting hiding ((%), now, text)
import qualified Formatting as F
import qualified Data.List.NonEmpty as NonEmpty
import Data.List ((\\))

  -- | Obtain a list of all members of a type universe, sans a finite list
universeSans
  :: Bounded x
  => Enum x
  => Ord x
  => [x]
  -> [x]
universeSans x = universe \\ x

class Reversing t where
  reversing :: t -> t

instance Reversing (NonEmpty a) where
  reversing = NonEmpty.reverse

reversed :: Reversing a => Iso' a a
reversed = involuted reversing

instance Reversing [a] where
  reversing = reverse

(%!) :: F.Format r a -> F.Format r' r -> F.Format r' a
(%!) = (F.%)

bothAnd ::
  a
  -> a
  -> (a -> Bool)
  -> Bool
bothAnd a1 a2 f = f a1 && f a2

-- | generalised version of `isPrefixOf` for when the lists are of different types
isPrefixOf' :: 
  (a -> b -> Bool)
  -> [a]
  -> [b]
  -> Bool
isPrefixOf' _ [] _ = True
isPrefixOf' _ (_:_) [] = False
isPrefixOf' eq (l:ls) (x:xs) = eq l x && isPrefixOf' eq ls xs

caseM
  :: Monad m
  => [MaybeT m a]
  -> m a
  -> m a
caseM cases fallback = runMaybeT (asum cases) >>= maybe fallback pure

wrap
  :: Semigroup a
  => a
  -> a
  -> a
wrap a b = a <> b <> a

composel
  :: Foldable f
  => f (a -> a)
  -> a
  -> a
composel = foldl' (.) id

isSuffixOf'
  :: Text
  -> Text
  -> Bool
isSuffixOf' a b = T.toLower a `T.isSuffixOf` T.toLower b

surroundM
  :: Monad m
  => m a -- ^ how to set it up
  -> m b -- ^ what to do
  -> (a -> m c) -- ^ how to take it apart again
  -> m b
surroundM pre doIt post = do
  p' <- pre
  r <- doIt
  _ <- post p'
  return r

(<$$>)
  :: Functor f
  => Functor g
  => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

prettyPrintList :: [Text] -> Text 
prettyPrintList [] = ""
prettyPrintList [x] = x
prettyPrintList [x, y] = x <> ", and " <> y
prettyPrintList (x:xs) = x <> ", " <> prettyPrintList xs


infixr 4 <<+~, <<-~

-- | Increment the target of a 'PermeableOptic' into your 'Monad''s state by a
-- number function and return the /old/ value that was replaced.
(<<+~)
  :: Num a
  => Optic A_Lens is s s a a
  -> a
  -> s
  -> (a, s)
(<<+~) l b s = (s ^. l, s & l %~ (+b))
{-# INLINE (<<+~) #-}

-- | Decrement the target of a 'PermeableOptic' into your 'Monad''s state by a
-- number function and return the /old/ value that was replaced.
(<<-~)
  :: Num a
  => Optic A_Lens is s s a a
  -> a
  -> s
  -> (a, s)
(<<-~) l b s = (s ^. l, s & l %~ (\x -> x - b))
{-# INLINE (<<-~) #-}

maybeOrReport2
  :: Monad m
  => Maybe a
  -> Maybe b
  -> m ()
  -> m ()
  -> (a -> b -> m c)
  -> m (Maybe c)
maybeOrReport2 c1 c2 err1 err2 f = do
    when (isNothing c1) err1
    when (isNothing c2) err2
    sequenceA (f <$> c1 <*> c2)

type MonadRS a m = (MonadReader a m, MonadState a m)