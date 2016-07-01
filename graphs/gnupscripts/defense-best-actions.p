set term pngcairo size 1366,768 enhanced font "Verdana"
set output "current/defense-best-actions.png"

set title "Max Fitness, pop: 50, selection: 45%, mutation: 75%, mutationFactor: 30"
set key outside
set xlabel "generations"
set xrange [1:50]
set ylabel "mean %"
set yrange [0:100]
plot "info/defenseActionsDist.txt" u 1 title "defense - Move"      with lines lw 3 smooth csplines,\
     "info/defenseActionsDist.txt" u 2 title "defense - Intercept" with lines lw 3 smooth csplines,\
     "info/defenseActionsDist.txt" u 3 title "defense - Catch"     with lines lw 3 smooth csplines,\
     "info/defenseActionsDist.txt" u 4 title "defense - NoOp"      with lines lw 3 smooth csplines