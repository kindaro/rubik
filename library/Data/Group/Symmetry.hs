{-# LANGUAGE UndecidableInstances #-}

module Data.Group.Symmetry where

import Prelude.Unicode

import Data.Type.Equality
import GHC.TypeLits

import Data.Finite
import Data.Finite.Simplex
import Data.Function
import Data.Group.Permutation

-- | This fancy type is isomorphous to the functions `Finite size → Finite size` that are invertible.
data Symmetry (size ∷ Natural) where
  SuccessorSymmetry ∷ (KnownNat size, size ~ ((size + 1) - 1)) ⇒ Symmetry size → Finite (size + 1) → Symmetry (size + 1)
  ZeroSymmetry ∷ Symmetry 1

infixr 8 :×
pattern (:×) ∷ (KnownNat size, size ~ ((size + 1) - 1)) ⇒ Finite (size + 1) → Symmetry size → Symmetry (size + 1)
pattern finite :× symmetry = SuccessorSymmetry symmetry finite

pattern Z ∷ Symmetry 1
pattern Z = ZeroSymmetry

class Equality α β where
  equality ∷ α → β → Bool

instance (KnownNat size₁, KnownNat size₂) ⇒ Equality (Finite size₁) (Finite size₂) where
  equality finite₁ finite₂ = getFinite finite₁ ≡ getFinite finite₂ ∧ natVal finite₁ ≡ natVal finite₂

instance (KnownNat size₁, KnownNat size₂) ⇒ Equality (Symmetry size₁) (Symmetry size₂) where
  equality ZeroSymmetry ZeroSymmetry = True
  equality (SuccessorSymmetry symmetry destination) (SuccessorSymmetry symmetry' destination') =
    equality symmetry symmetry' ∧ equality destination destination'
  equality _ _ = False

instance KnownNat size ⇒ Eq (Symmetry size) where
  symmetry₁ == symmetry₂ = equality symmetry₁ symmetry₂

instance Show (Symmetry size) where
  show ZeroSymmetry = "Z"
  show (SuccessorSymmetry symmetry destination) = (show ∘ getFinite) destination ++ ":×" ++ show symmetry

symmetryToPermutation ∷ ∀ size. KnownNat size ⇒ Symmetry size → Permutation (Finite size)
symmetryToPermutation ZeroSymmetry = mempty
symmetryToPermutation (SuccessorSymmetry previousSymmetry destination) =
  let Permutation{..} = symmetryToPermutation previousSymmetry
   in Permutation
        { to = \source → case strengthen source of
            Nothing → destination
            Just strongerSource → (face destination ∘ to) strongerSource
        , from = \source →
            if source ≡ destination
              then maxBound
              else (weaken ∘ from ∘ degeneracy destination) source
        }

class PermutationToSymmetry (size ∷ Natural) where
  permutationToSymmetry ∷ KnownNat size ⇒ Permutation (Finite size) → Symmetry size

instance {-# OVERLAPPING #-} PermutationToSymmetry 1 where
  permutationToSymmetry _ = ZeroSymmetry

instance (KnownNat n, KnownNat m, m ~ n + 1, n ~ m - 1, PermutationToSymmetry n) ⇒ PermutationToSymmetry m where
  permutationToSymmetry Permutation{..} =
    let recurse =
          permutationToSymmetry @n
            Permutation
              { to = degeneracy (to maxBound) ∘ to ∘ face maxBound
              , from = degeneracy (from maxBound) ∘ from ∘ face maxBound
              }
     in SuccessorSymmetry recurse (to maxBound)
