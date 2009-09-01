set terminal postscript eps enhanced
set border 0
set xtics 0
set ytics 0
set output "data/c-3120-simples.eps"
set nokey
set size .3,.3
plot [0:3][0:3] 'data/c-3120.dat' with lines lw 4
