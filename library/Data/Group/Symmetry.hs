module Data.Group.Symmetry where

import Prelude.Unicode

import GHC.TypeLits

import Data.Finite
import Data.Finite.Simplex
import Data.Group.Permutation

-- | This fancy type is isomorphous to the functions `Finite n → Finite n` that are invertible.
data Symmetry (n ∷ Natural) where
  SuccessorSymmetry ∷ KnownNat n ⇒ Symmetry n → Finite (n + 1) → Symmetry (n + 1)
  ZeroSymmetry ∷ Symmetry 0

symmetryToPermutation ∷ ∀ n. KnownNat n ⇒ Symmetry n → Permutation (Finite n)
symmetryToPermutation ZeroSymmetry = mempty
symmetryToPermutation (SuccessorSymmetry previousSymmetry whereLastOneGoes) =
  let Permutation{..} = symmetryToPermutation previousSymmetry
   in Permutation
        { from = \finite → case strengthen finite of
            Nothing → whereLastOneGoes
            Just smallerFinite → (face whereLastOneGoes ∘ from) smallerFinite
        , to = \finite → if finite ≡ whereLastOneGoes then maxBound else (weaken ∘ to ∘ degeneracy whereLastOneGoes) finite
        }
