set grid
set key font ",08"
set key left top

set term pdfcairo
f(x) = 0
g(x) = x
set xlabel 'step'
set ylabel 'x_N'
plot f(x), 'averages.dat' index 0 u 1:2 w l
set output 'x_N(300).pdf'
replot
unset output

set term pdfcairo
set ylabel 'X2_N'
plot g(x), 'averages.dat' index 1 u 1:2 w l
set output 'x2_N(300).pdf'
replot
unset output

