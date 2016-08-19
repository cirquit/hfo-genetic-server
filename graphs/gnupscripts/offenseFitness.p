set term pngcairo size 1366,768 enhanced font "Verdana"
set output "current/best-mean-fitness-offense.png"

set title "Max Fitness, pop: 100, gen: 100, episodes: 25, selection: 35%, mutation: 50%, mutationFactor: 20, single off player"
set key outside
set xrange [1:100]
set xlabel "Generation"
set ylabel "Fitness"
# set yrange [0:100]
plot "info/offenseFitness.dat" using 2 title 'mean offenseFitness' with lines lw 2 smooth csplines, \
     "info/offenseFitness.dat" using 1 title 'max offenseFitness'  with lines lw 2 smooth csplines

