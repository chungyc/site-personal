---
title: Highlighting
description: Page for experiments with syntax highlighting in Hakyll and Pandoc.
published: 2023-12-11
include-syntax-stylesheet: true
---

Page for experiments with syntax highlighting in Hakyll and Pandoc.
There is no useful information here.

## Haskell

```haskell
f :: Int -> Int
f x = x + 1
```

## Agda

```agda
module hello-world-proof where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

+-assoc : Set
+-assoc = ∀ (x y z : ℕ) → x + (y + z) ≡ (x + y) + z

+-assoc-proof : ∀ (x y z : ℕ) → x + (y + z) ≡ (x + y) + z
+-assoc-proof zero y z = refl
+-assoc-proof (suc x) y z = cong suc (+-assoc-proof x y z)
```
