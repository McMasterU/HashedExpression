\section{Polynomials:  Data Structures and Basic Algorithms}
\begin{code}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Polynomials where

import HashedExpression

--import Data.Complex
--import Data.Ratio
import qualified Data.IntMap.Strict as I
import qualified Data.Map.Strict as M
--import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
\end{code}

Names of variables and node numbers in source expression.
Invariant:  the IntMap and Map should be inverses of each other.
\begin{code}
data PolyEnv varLabel environment = PolyEnv (I.IntMap varLabel,M.Map varLabel Int) environment
  deriving (Show,Eq)
\end{code}

A polynomial is defined by an environment and a list of Terms.
The Terms must be sorted in descending order (leading first).
\begin{code}
data Poly a b = Poly (PolyEnv a b) [Term]  deriving (Show,Eq)
\end{code}

A Term is defined by a coefficient, total power and a map of variable number
(from environment) to powers, with all powers in the map being positive.
Invariant:  |I.foldr (+) $ snd $ snd m == fst $ snd m|.
\begin{code}

type Monomial = I.IntMap Int
type Term = (Rational,(Int,Monomial))


\end{code}

\begin{code}
instance Num [Term] where
  (+) t1s t2s = concatMap combineCoeffs
              $ L.groupBy (\ a b -> EQ == grevlex a b)
              $ sortTerms $ t1s ++ t2s

  (*) t1s t2s = concatMap combineCoeffs
              $ L.groupBy (\ a b -> EQ == grevlex a b)
              $ sortTerms $ [ termProd t1 t2 | t1 <- t1s, t2 <- t2s]


  negate t1s = map (\ (c,m) -> (negate c,m)) t1s
  abs _ts = error $ "abs of [Term]"
  signum _ts = error $ "signum of [Term]"
  fromInteger c = [(fromIntegral c,(0,I.empty))]

instance Num Term where
  (+) _t1 _t2 = error $ "+ of Term"
  (*) (c1,(d1,m1)) (c2,(d2,m2)) = (c1*c2,(d1+d2,I.unionWith (+) m1 m2))


  negate (c1,md) = (negate c1,md)
  abs _t = error $ "abs of Term"
  signum _t = error $ "signum of Term"
  fromInteger c = (fromIntegral c,(0,I.empty))

instance Fractional Term where
  (/) (c1,(d1,m1)) (c2,(d2,m2)) = (c1/c2,(d1-d2,I.unionWith (+) m1 (I.map negate m2)))

  fromRational q = (fromRational q,(0,I.empty))


combineCoeffs ts@((_,m1):_) = let cSum = sum $ map fst ts
                              in if cSum == 0 then [] else [(cSum, cleanPows m1)]
combineCoeffs [] = error "Polynomials.combineCoeffs of []"
cleanPows (d,ps) = (d,I.filter (/=0) ps)


instance Num a => Num (a,a) where
  (+) (x,y) (u,v) = (x+u,y+v)
  (*) (x,y) (u,v) = (x*u-y*v,x*v+y*u)
  negate (x,y) = (negate x, negate y)
  abs _z = error $ "abs of (,)"
  signum _z = error $ "signum of (,)"
  fromInteger c = (fromIntegral c, fromIntegral 0)

-- instance Ord Term where
--   (<=) a b = case grevlex a b of
--     GT -> False
--     _  -> True

instance HashedExpression.Complex ([Term],[Term]) [Term] where
  (+:) x y = (x,y)
  xRe (x,_y) = x
  xIm (_x,y) = y
  iRe x = (x,0)
  iIm y = (0,y)

instance HashedExpression.Complex (Term,Term) Term where
  (+:) x y = (x,y)
  xRe (x,_y) = x
  xIm (_x,y) = y
  iRe x = (x,0)
  iIm y = (0,y)


pAbs2 [] = 0
pAbs2 (t:prest) = (fromRational (fst t)) ** 2 + (pAbs2 prest)
\end{code}

Printing.
\begin{code}

pTerm (c,(totalDeg,degrees)) = concat $ [if I.foldr (+) 0 degrees == totalDeg then ""
                                           else "****TOTAL DEGREE MISMATCH****\n"
                                        ,case c of
                                            1 -> " + "
                                            -1 -> " - "
                                            _ -> (if c < 0 then " "++show c else " + "++show c) ++ " * "
                                        ,L.intercalate "*" $ map pPower $ I.toList degrees
                                        ]
pTerms terms = (L.intercalate " " $ map pPower $ L.sortBy grevlex terms)

pTerms' et = case et of
  (Left msg) -> msg ++ " pTerms'"
  (Right ts) -> pTerms ts

pPower (vIdx,pow) = case pow of
                       0 -> ""
                       1 ->  "t"++show vIdx
                       _ ->  "t"++show vIdx ++ "^"++show pow


--Generic printPolys
printPolys npolys = if length (L.nub $ map (\ (_,Poly polyEnv _) -> polyEnv) npolys) == 1
                   then unlines $ map pTerms npolys
                   else error "Polynomials.printPolys different environments!"
  where
    pTerms (n,Poly _ terms) = n ++" = "++ (L.intercalate " " $ map pTerm $ L.sortBy grevlex terms)





printPoly terms = L.foldr (++) "" (map pTerm terms)
\end{code}

--------------------------------------------
Term ordering function (grevlex).
--------------------------------------------

Order defined in (https://en.wikipedia.org/wiki/Term_order).
Graded reverse lexicographic order (grevlex) compares the total degree first,
then compares exponents of the variables in reverse order, reversing the outcome (so the Term with smaller exponent is larger in the ordering).
\begin{code}
grevlex (_,(t1,mon1)) (_,(t2,mon2)) = case compare t1 t2 of
                                        EQ -> fstNE $ map cmp allKeys
                                        x  -> x
    where
      fstNE (EQ:rest) = fstNE rest
      fstNE (x:_) = x
      fstNE [] = EQ

      cmp k = case (I.lookup k mon1, I.lookup k mon2) of
                (Nothing,Nothing) -> EQ
                (Just _, Nothing) -> GT          -- reversed
                (Nothing,Just _ ) -> LT          -- reversed
                (Just x, Just y ) -> compare x y -- reversed

      allKeys = reverse $ L.nub $ L.sort $ concatMap I.keys [mon1,mon2]
\end{code}


--------------------------------------------
Leading term of a polynomial (lt).
--------------------------------------------

Requires: Terms of poly input is ORDERED (descending, by term power)
(Could build this into function, but would be slightly less efficient).
'
\begin{code}

lt :: [Term] -> Either [Char] Term
lt [] = Left "Error:  zero polynomial in ..."
lt (t1:_ts) = Right t1

\end{code}

--------------------------------------------
Leading coefficient (lc).
--------------------------------------------

(Requires: Term terms of poly input is ordered.)
(Could build this into function, but would be slightly less efficient).
\begin{code}

lc p = case lt p of
  (Left err) -> Left $ err ++ " lc"
  (Right t) -> Right (fst t)

\end{code}

--------------------------------------------
Leading Term (lm).
--------------------------------------------

(Requires: Term terms of poly input is ordered.)
(Could build this into function, but would be slightly less efficient).
\begin{code}

lm p = case lt p of
  (Left err) -> Left $ err ++ " lm"
  (Right t) -> Right (snd t)

\end{code}


--------------------------------------------
Term Least Common Multiple (mlcm).
--------------------------------------------

\begin{code}
--mlcm m1 m2 = I.unionWith max m1 m2

mlcm (_,(_,t1mon)) (_,(_,t2mon)) = I.unionWith max t1mon t2mon

mlcm' f g = case f of
  (Left msg) -> Left (msg ++ " mlcm'")
  (Right okf) -> case g of
        (Left msg) -> Left (msg ++ " mlcm'")
        (Right okg) -> Right (1.0,(I.foldr (+) 0 mlcmfg, mlcmfg))
                        where mlcmfg = mlcm okf okg


\end{code}


--------------------------------------------
Helper functions.
--------------------------------------------

Find difference in power numbers of Terms.
\begin{code}

powDiff (_c1,(_d1,m1)) (_c2,(_d2,m2)) = I.filter (/=0) $ I.unionWith (+) (I.map negate m1) m2

\end{code}

Predicate: Input reflects no negative powers?
\begin{code}

noNegPow aMb = (I.foldr min 0 aMb) >= 0

\end{code}

Predicate: Does this Term divide that Term?
\begin{code}

termDiv t1 t2 = noNegPow $ powDiff t1 t2

\end{code}


--------------------------------------------
Find (c/lc(g)) * q * lm(g).
--------------------------------------------
m is a Term of f, where g dif m, and
m = q * lm(g).

Used for red1(f, g) = f - (c/lc(g)) * q * lm(g).
\begin{code}

-- FIXME: Currently, code calls "term" what is a "monomial."  Must therefore use
-- lt instead of lm to find lm.
-- A bunch of functions (along with a couple of type definitions)
-- must change if the nomenclature of this code
-- is to be made to conform with that of the theory's.

findqcDivlc :: [Term] -> [Term] -> Either [Char] Term
findqcDivlc [] _ = Left "got to empty list"
findqcDivlc _ [] = Left "empty divisor Term."
findqcDivlc (ft1:termsfr) g = case lt g of  -- ensuring lt(g) can be found, without errors?
  (Left err) -> Left (err ++ " findqm")
  (Right ltg@(lcg,_)) -> let diff = powDiff ltg ft1 -- ft1 is "m."  Check div of m
                                                    -- by "lm" of g (inside ltg).
                         in if (noNegPow diff)
                            then Right (fst ft1 / lcg, addTotDeg diff) -- fst ft1 == c
                                                                       -- (addTotDeg diff)
                                                                       -- == q * g ?  CHECK
                            else findqcDivlc termsfr g

addTotDeg im = (I.foldr (+) 0 im, im)
\end{code}

--------------------------------------------
redBasis and red1.
--------------------------------------------
red1: (initially) For Buchberger (used in polynomial reduction) -- https://en.wikipedia.org/wiki/Gröbner_basis#Reduction
redBasis: Perform red1 (iteratively) until result is irreducible by G = some set of gs.

\begin{code}

-- Perhaps use neighbouhood instead of ==.
redBasis f gs eps = let f1 = foldr (flip red1) f gs -- Q:look at this (with flip) more carefully.
--                in if f == f1 then f   -- N:Replaced by line below (rounding problems)
          in if ((pAbs2 f) - (pAbs2 f1)) < eps then f -- Q:does this make sense?  Check pAbs2
                              else redBasis f1 gs eps -- N:degree implies it cannot recurse infinitely

redBasis' f gs eps = case gs of
  (Left msg) -> Left (msg ++ " redBasisPrime")
  (Right isgs) -> Right (redBasis f isgs eps)

-- try to reduce f by g.  If locally irreducible, return original f.
red1 f g = case findqcDivlc f g of
             Left _ -> f
             Right cDivlcqg -> f - (map ((*) cDivlcqg) g)



--kredBasis k f gs
\end{code}


--------------------------------------------
Term and Polynomial products.
--------------------------------------------

(Still needed?-->)Requires: Ordered [t] by fst t.

\begin{code}
sortTerms terms = L.sortBy (flip grevlex) terms
termProd (c1,(d1,m1)) (c2,(d2,m2)) = (c1*c2, (d1+d2,I.unionWith (+) m1 m2))
--monoPolyProd mono (Poly env monos) = Poly env $ sortMonos $ map (monoProd mono) monos
\end{code}


--------------------------------------------
Helper Functions: Buchberger's Algorithm.
--------------------------------------------

\begin{code}
--S-polynomial of two polynomials.

spol :: [Term] -> [Term] -> Either [Char] [Term]

spol f g = let  eltf = lt f
                eltg = lt g
           in case (mlcm' (eltf) (eltg)) of
                (Left msg) -> Left (msg ++ " spol")
                (Right mlcmp) -> case (eltf) of
                            (Left msg) -> Left (msg ++ " lt f")
                            (Right ltf) -> case (eltg) of
                                (Left msg) -> Left (msg ++ " lt g")
                                (Right ltg) -> Right ([mlcmp/ltf] * f - [mlcmp/ltg] * g)

\end{code}


--------------------------------------------
Buchberger's Algorithm.
(http://www.scholarpedia.org/article/Buchberger's_algorithm)
--------------------------------------------

\begin{code}

--buchberger' :: [([Term],[Term])] -> [[Term]] -> [[Term]]

buchberger' [] gs _ = Right gs
buchberger' _ [] _ = Right []
buchberger' ((f, g):restC) gs eps = case (spol f g) of
  (Left msg) -> Left (msg ++ "  buchbergrer'.")
  (Right spolfg) -> case (redBasis spolfg gs eps) of
        [] -> buchberger' restC gs eps
        (hd:rest) -> buchberger'
                (restC++([(g',(hd:rest)) | g' <- gs])) (gs++[(hd:rest)])
                eps

buchberger gs = buchberger' [(g1,g2) | g1 <- gs, g2 <- gs] gs globalEps




termFromTriple :: (Rational,Int,Int) -> Term
termFromTriple (c,v,e) = (c,(e,I.fromList [(v,e)]))

polyFromTriples [] = []
polyFromTriples (t:rest) = (termFromTriple t):(polyFromTriples rest)

globalEps = 0.0000001
\end{code}
