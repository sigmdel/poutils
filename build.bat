@echo off
FOR %%S IN (poclean pocopy pofill poignore poinfo pomerge posort poscrub postrip poswap poupdate) DO (
C:\lazarus\lazbuild -B -q --bm=Release %%S.d\%%S.lpi
move %%S.d\%%S.exe bin\
rd /S /Q %%S.d\lib
)
