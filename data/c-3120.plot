set terminal postscript eps enhanced
set ylabel "pitch"
set xlabel "time"
set xtics 1
set ytics 1
set output "data/c-3120.eps"
set grid
set nokey
set size .3,.3
plot [0:3][0:3] 'data/c-3120.dat' with linespoints lw 4
