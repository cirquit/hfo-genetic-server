set term pngcairo size 1366,768 enhanced font "Verdana"
set output "current/offenseActionDist.png"

set title "mean-max fitness, pop: 50, gen: 50, episodes: 25, selection: 35%, mutation: 50%, mutationFactor: 20, 1v1, actiondistribution"
set key outside
set xrange [1:50]
set xlabel "Generation"
set ylabel "%"
set yrange [0:100]
plot "info/offenseActDist.dat" using 1 title 'E(Move)'      with lines lw 2 smooth csplines, \
     "info/offenseActDist.dat" using 2 title 'E(Intercept)' with lines lw 2 smooth csplines, \
     "info/offenseActDist.dat" using 3 title 'E(Shoot)'     with lines lw 2 smooth csplines, \
     "info/offenseActDist.dat" using 4 title 'E(Dribble)'   with lines lw 2 smooth csplines, \
     "info/offenseActDist.dat" using 5 title 'E(NoOp)'      with lines lw 2 smooth csplines

