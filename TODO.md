### TODO
- This took very long time to finish 
    - Update 15/07/19 : Much faster
```haskell
main = do
    let exp1 = (((((n+:l))^3))^3)
    let exp2 = ((k+:u)+(p+:j))
    showExp $ simplify $ exp1 * exp2
```
- Have a collections of lots of expressions for testing 
- makeRecursive use topological sort
