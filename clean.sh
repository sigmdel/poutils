#!/usr/bin/bash

for src in poclean pocopy pofill poignore poinfo pomerge poscrub posort postrip poswap poupdate
do 
  rm $src.d/$src
  rm $src.d/$src.lps
  rm -r $src.d/backup
  rm -r $src.d/lib
done  
