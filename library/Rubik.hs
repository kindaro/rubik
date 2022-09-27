module Rubik where

import Prelude.Unicode

import Data.Bifunctor
import Data.Either
import Data.Function
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.TypeLits

import Data.Finite
import Data.Group.Finite
import Data.Group.Permutation

import Data.Finite.Simplex
import Data.Group.Symmetry

-- I need to make sure… that stuff is good.
-- A cube is 27 cubies. They can move about and each can be rotated. So I need S₂₇ × (S₄ × ℤ/2ℤ)²⁷.
-- I can encode this appropriately.
-- I can then define my actions. There are only 6 actions and I shall have to enumerate them.
-- Now I can do search in the space of actions. This is familiar. I can even factor out the engine.

-- what are the symmetries?

data SymmetryOfCube = SymmetryOfCube
  { direction ∷ Finite 6
  , rotation ∷ Finite 4
  , orientation ∷ Finite 2
  }

iterator ∷
  ∀ monad underling.
  (Ord underling, Monad monad) ⇒
  (underling → Set (Either underling underling)) →
  underling →
  (Set underling → Set underling → Set underling → monad ()) →
  monad [Set underling]
iterator grow seed reportProgress = worker (Set.singleton seed)
 where
  worker ∷ Set underling → monad [Set underling]
  worker = fix \recurse growth →
    if Set.null growth
      then return []
      else do
        let (berries, sprouts) = (partition ∘ Set.unions ∘ Set.map grow) growth
        reportProgress growth berries sprouts
        lifeGoesOn ← recurse sprouts
        return (berries : lifeGoesOn)

partition ∷ (Ord left, Ord right) ⇒ Set (Either left right) → (Set left, Set right)
partition = bimap Set.fromList Set.fromList ∘ partitionEithers ∘ Set.toList
