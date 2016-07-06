set term pngcairo size 1366,768 enhanced font "Verdana"
set output "current/offense-best-actions.png"

set title "Max Fitness, pop: 20, gen: 20, selection: 45%, mutation: 75%, mutationFactor: 30"
set key outside
set xlabel "generations"
set xrange [1:50]
set ylabel "mean %"
set yrange [0:100]
plot "info/offenseActionsDist.txt" u 1 title "offense - Move"       with lines lw 2 smooth csplines,\
     "info/offenseActionsDist.txt" u 2 title "offense - Intercept"  with lines lw 2 smooth csplines,\
     "info/offenseActionsDist.txt" u 3 title "offense - Catch"      with lines lw 2 smooth csplines,\
     "info/offenseActionsDist.txt" u 4 title "offense - NoOp"       with lines lw 2 smooth csplines,\
     "info/offenseActionsDist.txt" u 5 title "offense - Shoot"      with lines lw 2 smooth csplines,\
     "info/offenseActionsDist.txt" u 6 title "offense - Dribble"    with lines lw 2 smooth csplines
