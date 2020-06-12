# Generating haddock
- from project root
```terminal
stack haddock --haddock-arguments "--odir=docs/"
```
# TODOs
### TODO Write Docs - Start With HashedPattern
- Internal/Expression.hs
- Internal/Inner.hs
- Internal/Pattern.hs
- Internal/Normalize.hs *Chris*
- Internal/CollectDifferential.hs
- Internal/Node.hs
- Operation.hs *Steven*

- Value.hs *Nasim*
- Interp.hs *Nasim*
- Derivative.hs

- Value.hs *Chris*
- Problem.hs *Chris*

- Codegen.hs *Chris*
- Codegen/CSimple.hs *Steven*

- Prettify.hs
### FIXME haddock module descriptions
- add proper copyright info
### TODO better symphony example in README
### TODO collapse ToShape into Dimension (leave toShape method)
### TODO add regression tests for examples
### TODO Better interface for Transformation's (in Inner.hs) needed? 
- Make relationship between Transformation/Modification/Change clearer? Put in it's own module?
- toRecursiveSimplification and toRecursiveCollecting should be reduced to one function?
### TODO Make sure we don't introduce bugs doing CodeGen for FT
### TODO Maybe use Numeric Prelude for better Num class and then better VectorSpace
### TODO add cVariable1D name = variable1D (name ++ "Re") +: variable1D (name ++ "Im"), cVariable2D = ...
### TODO Rename Node to Op and Internal to Node
### TODO CollectDifferntial shouild just be moved to Derivative??
### TODO Partial needs to be moved into Derivative and deleted
### TODO Interp haddock needs refining
### TODO Operation haddock needs refining
