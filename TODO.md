### TODO
- This took very long time to finish
```haskell
main = do
    let exp1 = (((((n+:l))^3))^3)
    let exp2 = ((k+:u)+(p+:j))
    showExp $ simplify $ exp1 * exp2
```

- Have a collections of lots of expressions for testing 
- Avoid merging expressions when simplifying to speed things up
