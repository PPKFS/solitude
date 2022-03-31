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
, isPrefixOf'
, caseM
, wrap
, composel
, isSuffixOf'
, surroundM
, (<$$>)
, prettyPrintList
, bothAnd) where

import Relude
import Optics hiding (uncons)
import Optics.State.Operators
import qualified Data.Text as T
import Relude.Extra.Bifunctor
import Relude.Extra.Tuple

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
