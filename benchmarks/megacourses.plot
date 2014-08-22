#!/usr/bin/gnuplot
#
# Creates a version of a plot, which looks nice for inclusion on web pages
#
# AUTHOR: Hagen Wierstorf

reset

set terminal epslatex size 4.5, 3.62 color colortext
set output "megacourses.tex"

set style line 11 lc rgb '#808080' lt 1
set border 3 back ls 11
set tics nomirror

set style line 12 lc rgb '#808080' lt 0 lw 1
set grid back ls 12

# color definitions
set style line 1 lc rgb '#8b1a0e' pt 1 ps 1 lt 1 lw 2 # --- red
set style line 2 lc rgb '#5e9c36' pt 6 ps 1 lt 1 lw 2 # --- green

set key at graph 0.7,0.95

set xlabel 'Courses'
set ylabel 'Seconds'
set xrange [50:65]
set yrange [0:1000]

plot 'megacourses.dat' u 1:2 t 'With busy cuts' w lp ls 1, \
     ''            u 1:3 t 'Without busy cuts' w lp ls 2
