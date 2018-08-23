# set terminal png transparent nocrop enhanced size 450,320 font "arial,8" 
# set output 'scatter.1.png'
set dummy u, v
set key inside right top vertical Right noreverse enhanced autotitle box lt black linewidth 1.000 dashtype solid
set parametric
set title "Simple demo of scatter data conversion to grid data" 
set xlabel "data style point - no dgrid" 
u = 0.0
## Last datafile plotted: "hemisphr.dat"
splot "ex6data2.txt"
pause -1 "Hit any key to continue"
