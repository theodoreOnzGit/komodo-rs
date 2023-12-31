---
title: %MTEM
theme: _config.yml
filename: mtem
---

# %MTEM Card

This card is used to input moderator or coolant temperature parameters.

| `%MTEM` | Variable | Description | Remarks |
| --- | --- | --- | --- |
| LINE 1 | CMTEM | Uniform distribution of the moderator temperature in Kelvin | Used as guess if `%THER` card active |
|     | RMTEM | Moderator temperature reference in Kelvin from which the interpolation is done | Dummy if `%XTAB` card active |
| LINE 2 | MISGTR(g) | Macroscopic Cross Section changes due to changes of moderator temperature in Kelvin  | Repeat LINE 2 NG times. And again repeat this input segment NMAT times. **This line is not necessary if `%XTAB` card present** |
|   | MSIGA(g) |
|   | MNUF(g) |
|   | MSIGF(g) |
|   | MSIGS(g,1:NG) |
| LINE 3 | POPT | Print option if users want to print this card | Optional |

Example:
```
! MODERATOR TEMPERATURE CARD
%MTEM
559.19  579.75  ! Average Moderator Temperature and Moderator temperature Ref. Kelvin
! CX change per Moderator Temperature Changes
!  sigtr          siga         nu*sigf     kappa*sigf  sigs_g1   sigs_g2
 0.00000E+00   0.00000E+00   0.00000E+00   0.00000E+00  0.0  0.00000E+00
 0.00000E+00   0.00000E+00   0.00000E+00   0.00000E+00  0.0  0.00000E+00    !COMP 1
 0.00000E+00   0.00000E+00   0.00000E+00   0.00000E+00  0.0  0.00000E+00
 0.00000E+00   0.00000E+00   0.00000E+00   0.00000E+00  0.0  0.00000E+00    !COMP 2
 0.00000E+00   0.00000E+00   0.00000E+00   0.00000E+00  0.0  0.00000E+00
 0.00000E+00   0.00000E+00   0.00000E+00   0.00000E+00  0.0  0.00000E+00    !COMP 3
-2.03310E-06   2.12191E-07   1.24709E-07   1.43035E-18  0.0  8.09676E-07
-1.08674E-04  -3.15597E-05  -4.16439E-05  -5.46722E-16  0.0  0.00000E+00    !COMP 4
-1.98080E-06   2.26000E-07   1.35145E-07   1.56896E-18  0.0  8.58474E-07
-9.06150E-05  -3.21435E-05  -4.53102E-05  -5.94857E-16  0.0  0.00000E+00    !COMP 5
-1.92434E-06   2.39939E-07   1.49084E-07   1.75422E-18  0.0  9.03494E-07
-7.62786E-05  -3.23776E-05  -4.78475E-05  -6.28174E-16  0.0  0.00000E+00    !COMP 6
-2.69634E-06   2.48530E-07   1.40773E-07   1.56896E-18  0.0  7.01311E-07
-7.62435E-05  -3.00119E-05  -4.20202E-05  -5.51669E-16  0.0  0.00000E+00    !COMP 7
-3.07905E-06   2.61854E-07   1.43235E-07   1.67897E-18  0.0  6.17380E-07
-7.33397E-05  -2.91929E-05  -4.07701E-05  -5.35261E-16  0.0  0.00000E+00    !COMP 8
-3.53877E-06   2.74313E-07   1.46019E-07   1.71665E-18  0.0  5.16547E-07
-7.13711E-05  -2.83041E-05  -3.94319E-05  -5.17689E-16  0.0  0.00000E+00    !COMP 9
-2.63907E-06   2.64289E-07   1.55858E-07   1.75422E-18  0.0  7.44320E-07
-6.39554E-05  -3.03509E-05  -4.44431E-05  -5.83483E-16  0.0  0.00000E+00    !COMP 10
-3.02147E-06   2.79060E-07   1.58814E-07   1.88528E-18  0.0  6.59521E-07
-6.16984E-05  -2.95626E-05  -4.31588E-05  -5.66622E-16  0.0  0.00000E+00    !COMP 11
1
```
