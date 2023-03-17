---
title: Trying out Diagrams
description: Trying out Diagrams to embed images in the web site.
published: 2023-03-20
toc: true
include-syntax-stylesheet: true
---

I'm trying out using [Diagrams] for including diagrams.  Let's see if this works.

[Diagrams]: https://diagrams.github.io/

## First diagram

The following is the first diagram I tried:

```haskell
myCircle :: Diagram B
myCircle = circle 1 # fc red

main = putDiagram defaultOptions myCircle
```

It is a circle filled with the color red.

![First diagram](/diagrams/article/diagrams/first.svg)

## Combining diagrams

A circle atop another:

```haskell
example :: Diagram B
example = circle 2 # lc purple `atop` circle 1 # lc green
```

![Circle atop another](/diagrams/article/diagrams/combine1.svg)

A circle and square side by side:

```haskell
example :: Diagram B
example = circle 1 ||| square 2
```

![Circle and square side by side](/diagrams/article/diagrams/combine2.svg)

A circle stacked on above a square:

```haskell
example :: Diagram B
example = circle 1 === square 2
```

![Circle above square](/diagrams/article/diagrams/combine3.svg)

Three series of growing circles:

```haskell
circles :: Diagram B
circles = hcat $ map circle [1..6]

example :: Diagram B
example = vcat $ replicate 3 circles
```

![Rows of growing circles](/diagrams/article/diagrams/combine4.svg)

## Aligning shapes

Circles of different sizes aligned on the top:

```haskell
example = hrule (2 * sum sizes) === circles # centerX
  where
    circles = hcat . map alignT . zipWith scale sizes $ repeat $ circle 1
    sizes = [2, 5, 4, 7, 1, 3]
```

![Shapes aligned on top](/diagrams/article/diagrams/combine4.svg)

## See also

*   [Diagrams Quick Start Tutorial](https://diagrams.github.io/doc/quickstart.html)
