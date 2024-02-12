set grid
set key font ",08"
set key left top

set term pdfcairo
set ylabel 'MSD'
h(x) = a*x**(2*b)
a = 1
b = 1
fit h(x) 'averages.dat' index 2 via a, b
set logscale x
set logscale y
plot h(x), 'averages.dat' index 2 w l
set output 'MSD(300).pdf'
replot
unset output
