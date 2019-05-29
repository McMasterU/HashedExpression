{-
(c) 2010 Christopher Kumar Anand

Utility functions for Hashed Expresson
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HashedUtils where

import HashedExpression

import Data.Complex as DC
import qualified Data.List as L
import qualified System.Exit
import qualified System.Process

{-

Save data to files.
-}
saveFloats :: (Show a) => String -> [a] -> IO System.Exit.ExitCode
saveFloats fileName l =
    do saveInDataFile fileName l
       writeFile "genData.c" (concat $ L.intersperse "\n" lines)
       >> (System.Process.system "gcc genData.c -o genData.o") >>
    (System.Process.system "time ./genData.o") >>
    (System.Process.system ("rm " ++ fileName ++ ".dat"))
  where
    lines =
        [ "#include <stdlib.h>"
        , "#include <stdio.h>"
        , "\nmain(void) {"
        , "FILE *fp_in,*fp_out;"
        , "char line[100];"
        , "int i,size;"
        , "float *data = malloc(" ++ show (length l) ++ "*sizeof(float)) ;"
        , "fp_in = fopen(\"" ++ fileName ++ ".dat\",\"r\");"
        , "for (i=0; i<" ++ show (length l) ++ "; i++)"
        , "  fscanf(fp_in,\"%f\", &data[i],line);"
        , "fp_out=fopen(\"" ++ fileName ++ "\", \"w\");"
        , "fwrite(data,4," ++ show (length l) ++ ",fp_out);"
        , "fclose(fp_in);"
        , "fclose(fp_out);"
        , "exit(0);\n}\n"
        ]

{-


Create a binary data file out of a list.  Data is recorded in fileName.
-}
saveInC fileName l dims =
    do saveInDataFile fileName l
       writeFile "genData.c" (concat $ L.intersperse "\n" lines)
       >> (System.Process.system "gcc genData.c -o genData.o") >>
    (System.Process.system "time ./genData.o")
  where
    prodDims =
        case dims of
            Dim4 (dimX, dimY, dimZ, dimW) -> dimX * dimY * dimZ * dimW
            Dim3 (dimX, dimY, dimZ) -> dimX * dimY * dimZ
            Dim2 (dimX, dimY) -> dimX * dimY
            Dim1 (dimX) -> dimX
            Dim0 -> 1
            x -> error $ "saveInC doesn't support " ++ show x
    lines =
        [ "#include <stdlib.h>"
        , "#include <stdio.h>"
        , "\nmain(void) {"
        , "FILE *fp_in,*fp_out;"
        , "char line[100];"
        , "int i,size;"
        , "double *data = malloc(" ++ show prodDims ++ "*sizeof(double)) ;"
        , "fp_in = fopen(\"" ++ fileName ++ ".dat\",\"r\");"
        , "size=" ++ show prodDims ++ ";"
        , "for (i=0; i<size; i++)"
        , "  fscanf(fp_in,\"%lf\", &data[i],line);"
        , "fp_out=fopen(\"" ++ fileName ++ "\", \"w\");"
        , "fwrite(data,8,size,fp_out);"
        , "fclose(fp_in);"
        , "fclose(fp_out);"
        , "exit(0);\n}\n"
        ]

saveInDataFile fileName l = do
    writeFile (fileName ++ ".dat") (concat $ L.intersperse "\n" (map show l))

{-

-}
mate name =
    System.Process.system $
    "open -a ~/Downloads/TextMate.app " ++ name ++ ".lhs"

search string = System.Process.system $ "fgrep \"" ++ string ++ "\" *.lhs"

fromReal :: (RealFloat a) => a -> DC.Complex a
fromReal x = DC.mkPolar x 0
{-

-}
