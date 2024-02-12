set grid
set key font ",08"
set key left top

set term pdfcairo
set ylabel 'dR'
set xlabel 'step'
f(x) = x
plot f(x), 'dR_lattice.dat' u 1:2 w l
set output 'dR_lattice(100)(200).pdf'
replot
unset output

set term pdfcairo
set ylabel 'dR'
set xlabel 'step'
h(x) = a*x**(2*b)
a = 1
b = 1
fit h(x) 'dR_lattice.dat' via a, b
set logscale x
set logscale y
plot h(x), 'dR_lattice.dat' w l
set output 'dR_lattice_fit(100)(200).pdf'
replot
unset output
