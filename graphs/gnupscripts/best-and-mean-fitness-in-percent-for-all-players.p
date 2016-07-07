set term pngcairo size 1366,768 enhanced font "Verdana"
set output "current/best-and-mean-fitness-in-percent-for-all-players.png"

set title "Max Fitness, pop: 50, gen: 50, selection: 35%, mutation: 50%, mutationFactor: 20"
set key outside
set xrange [1:50]
set xlabel "Generation"
set ylabel "%"
set yrange [0:100]
plot "info/offenseContent.txt" using 2 title 'mean offenseFitness' with lines lw 2 smooth csplines, \
     "info/offenseContent.txt" using 1 title 'max offenseFitness'  with lines lw 2 smooth csplines

