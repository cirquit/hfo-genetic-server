set term pngcairo size 1366,768 enhanced font "Verdana"
set output "current/best-and-mean-fitness-in-percent-for-all-players.png"

set title "Max Fitness, pop: 50, selection: 45%, mutation: 75%, mutationFactor: 30"
set key outside
set xrange [1:50]
set xlabel "Generation"
set ylabel "%"
set yrange [0:100]
plot "info/defenseContent.txt" using 2 title 'mean defenseFitness' with lines lw 2 smooth csplines, \
     "info/defenseContent.txt" using 1 title 'max defenseFitness'  with lines lw 2 smooth csplines, \
     "info/offenseContent.txt" using 2 title 'mean offenseFitness' with lines lw 2 smooth csplines, \
     "info/offenseContent.txt" using 1 title 'max offenseFitness'  with lines lw 2 smooth csplines

