{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}


module Maker.Prelude (module Maker.Prelude, module X) where

import Prelude.Unicode as X ((⊥))
import Data.Bool.Unicode as X ((∧), (¬)) 
import Prelude as X (
  -- Conversion to and from strings
  Read(..), Show(..), read,
  -- Comparisons
  Eq(..), Ord(..),
  -- Core abstractions
  Functor (fmap),
  Applicative (),
  Monad (return , (>>=)),
  -- Mumeric classes
  Num(..), Integral (), Enum (),
  -- Numeric conversions
  Real(..), Fractional(..),
  RealFrac(..),
  fromIntegral,
  -- Simple types
  Integer, Int, String,
  -- Algebraic types
  Bool (True, False),
  Maybe (Just, Nothing),
  Either (Right, Left),
  -- Functional operators
  (.), ($),
  -- Numeric operators
  (+), (-), (*), (/), (^), (^^), div,  -- ^ for the Knuth uparrow
  
  -- Utilties
  all, not, elem, (&&),
  -- Constants
  mempty, otherwise)
  
import Control.Monad.State as X (
  StateT, -- Type constructor that adds state to a monad type
  execStateT,  -- Runs a state monad with given initial state
  get,  -- Gets the state in a do block
  put)  -- Sets the state in a do block

import Control.Monad.Writer as X (
  WriterT,  -- Type constructor that adds state to a monad type
  Writer, -- Tyupe constructor of logging methods
  runWriterT, -- Runs a writer monad transformer
  execWriterT) -- Runs a writer monad transformer keeping only logs

import Control.Monad.Except as X (
  MonadError, -- Type class of monads that fail
  Except, --Type constructor of failing monads
  throwError, --Short-circuits the monadic computation
  runExcept) -- Runs a failing monad

import Data.Fixed as X (
  Fixed (..), -- Type constructor for numbers of given precision
  HasResolution (..)) -- Type class for specifying precisions

import Control.Lens as X (
  Lens', lens,
  makeLenses, -- Defines lenses for record fields,
  makeFields, -- Defines lenses for record fields,
  set, -- Writes a lens
  use, preuse, -- Read a lens from a state value
  view, -- Read a lens from a value
  ix, -- Lens for map retrieval and updating
  at, -- Lens for map insertiton
  -- Operators for partial state up[dateds in do blocks:
  (.=), -- Replacs  -- Note that in the purple book, the symbol is :=
  (-=), (+=), -- Update arithmetically
  (%=), -- Update according to function
  (?=)) -- insert into Map

import Control.Lens.Zoom as X (zoom)

import Data.Map as X (
  Map, -- Type constructor for mapping
  empty, -- polymorphic empty mapping,
  singleton, --creates a mapping with a single key -value pairs
  fromList) -- Creates a mapping with several key-value pairs


decrease a x = a -= x
increase a x = x += a
initialize a x = a %= (\ case Nothing → Just x; y → y)
prepend a x = a %= (x:)
x ∉ xs = (¬) (elem x xs)
