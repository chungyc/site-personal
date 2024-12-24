---
title: Symtegration
description: I have released Symtegration, which is a Haskell library for symbolically integrating mathematical expressions.
published: 2024-12-24
---

I have been on a symbolic integration kick for the past month,
which has ended up with me making the first release for [Symtegration].
It is a Haskell library for symbolically integrating mathematical expressions.
At the current time, it only supports basic integration,
but I hope to eventually implement most of the algorithms in
[Symbolic Integration I: Transcendental Functions],
although that will take be many years into the future.
(Sadly, there will never be a part 2, at least not by the same author.)

My secret success criteria for the project would be if I can do all computations for
a basic quantum mechanics course within just the [GHCi] interpreter.
It could even be used within an [IHaskell] notebook, with the
derived integrals being typeset with LaTeX.

Why "Symtegration"?  "**Sym**bolic in**tegration**".
It may or may not be the greatest of names,
but no one else seemed to be using it as a name for something,
and it was a simple enough name.

[Symtegration]: https://github.com/chungyc/symtegration

[Symbolic Integration I: Transcendental Functions]: https://doi.org/10.1007/b138171

[GHCi]: https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html

[IHaskell]: https://github.com/IHaskell/IHaskell
