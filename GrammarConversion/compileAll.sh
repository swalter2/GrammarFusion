#!/bin/bash
cd ABNF 
./compile.sh
cd ../Fragments
./compile.sh
cd ..
ghc -iABNF -iFragments --make Main.hs -o convert
