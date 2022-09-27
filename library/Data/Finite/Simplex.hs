module Data.Finite.Simplex where

import GHC.TypeLits

import Data.Finite

face ∷ Finite (n + 1) → Finite n → Finite (n + 1)
face threshold vertex =
  let weakerVertex = weaken vertex
  in if weakerVertex < threshold then weakerVertex else shift vertex

degeneracy ∷ KnownNat n ⇒ Finite (n + 1) → Finite (n + 1) → Finite n
degeneracy threshold vertex = case (strengthen vertex, unshift vertex) of
  (_, Nothing) → 0
  (Nothing, Just smallerVertex) → smallerVertex
  (Just strongerVertex, Just smallerVertex) → if vertex < threshold then strongerVertex else smallerVertex
