set key font ",05"
set key left top
set grid

set term pdfcairo
f(x) = 0
g(x) = x
set xlabel 'step'
set ylabel 'x_N'
plot f(x), for [IDX=0:9] 'x_N.dat' index IDX u 1:2 w l lt IDX+1
set output 'x_N(4).pdf'
replot
unset output
set term pdfcairo
set ylabel 'x2_N'
plot g(x), for [IDX=0:9] 'x2_N.dat' index IDX u 1:2 w l lt IDX+1
set output 'x2_N(4).pdf'
replot
unset output
set term pdfcairo
set ylabel 'MSD'
plot g(x), for [IDX=0:9] 'MSD.dat' index IDX u 1:2 w l lt IDX+1
set output 'MSD(4).pdf'
replot
unset output
set term pdfcairo
set xlabel 'walkers'
set ylabel 'delta'
plot 'delta.dat' u 1:2 w l
set output 'delta(4).pdf'
replot
unset output
