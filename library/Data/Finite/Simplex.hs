module Data.Finite.Simplex where

import GHC.TypeLits

import Data.Finite

face ∷ Finite (size + 1) → Finite size → Finite (size + 1)
face threshold vertex =
  let weakerVertex = weaken vertex
   in if weakerVertex < threshold then weakerVertex else shift vertex

degeneracy ∷ KnownNat size ⇒ Finite (size + 1) → Finite (size + 1) → Finite size
degeneracy threshold vertex = case (strengthen vertex, unshift vertex) of
  (_, Nothing) → 0
  (Nothing, Just smallerVertex) → smallerVertex
  (Just strongerVertex, Just smallerVertex) → if vertex < threshold then strongerVertex else smallerVertex
