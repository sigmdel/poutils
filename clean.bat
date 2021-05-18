@echo off
FOR %%S IN (poclean pocopy pofill poignore poinfo pomerge posort poscrub postrip poswap poupdate) DO (
del %%S.d\%%S.exe
del %%S.d\%%S.lps
killdir %%S.d\backup
killdir %%S.d\lib
)
