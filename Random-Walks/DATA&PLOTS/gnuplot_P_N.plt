set grid
set key font ",08"
set key left top

set term pdfcairo
set xlabel 'x'
set ylabel 'P_N'
#set xrange [-9:9]
plot 'P_N.dat' index 0 u 1:2 with boxes
replot 'P_N.dat' index 1 u 1:2 w l
#plot for [IDX=0:1] 'P_N.dat' index IDX u 1:2 with boxes lt IDX+1
set output 'P_N(12)(boxes).pdf'
replot
unset output
