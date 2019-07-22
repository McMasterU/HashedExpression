### TODO
- This took very long time to finish 
    - Update 15/07/19 : Much faster
```haskell
main = do
    measureTime $ do
        let exp1 = (((((n+:l))^3))^3)
        let exp2 = ((k+:u)+(p+:j))
        showExp $ simplify $ exp1 * exp2
```
- Have a collections of lots of expressions for testing 
- makeRecursive use topological sort

- Fix bug: 15/07/2019: This is not correct
```haskell
    [ (s *. x) <.> y |.~~~~~~> s * (x <.> y)
    , x <.> (s *. y) |.~~~~~~> s * (x <.> y)
    ]
```
Changed to 
```haskell
    [ (s *. x) <.> y |.~~~~~~> s *. (x <.> y)
    , x <.> (s *. y) |.~~~~~~> s *. (x <.> y)
    ]
```
- Update 18/07: The rules above changed to:
```haskell
    [ (s *. x) <.> y |.~~~~~~> s *. (x <.> y) --
    , x <.> (s *. y) |. isReal s ~~~~~~> s *. (x <.> y)
    , x <.> ((z +: t) *. y) |.~~~~~~> (z +: negate (t)) *. (x <.> y) -- Conjugate if the scalar is complex
    , x <.> y |. (isScalar x &&. isScalar y) &&. (isReal x &&. isReal y) ~~~~~~>
      (x * y)
    ]
```
because the conjugation of complex numbers

