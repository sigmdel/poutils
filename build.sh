#!/usr/bin/bash

for src in poclean pocopy pofill poignore poinfo pomerge posort postrip poswap poupdate
do 
  lazbuild -B -q --bm=Release $src.d/$src.lpi
  mv $src.d/$src bin/
  rm -r $src.d/lib
done  
