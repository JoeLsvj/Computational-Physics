set grid
set key font ",06"
set key left top

set term pdfcairo
f(x) = 0
g(x) = x
set xlabel 'step'
set ylabel 'ix'
plot f(x), for [IDX=0:4] 'ix.dat' index IDX u 1:2 w l lt IDX+1
set output 'ix.pdf'
replot
unset output
set term pdfcairo
set ylabel 'ix^2'
plot g(x), for [IDX=0:4] 'ix2.dat' index IDX u 1:2 w l lt IDX+1
set output 'ix2.pdf'
replot
unset output
