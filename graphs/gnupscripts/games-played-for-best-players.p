set term pngcairo size 1366,768 enhanced font "Verdana"
set output "current/games-played-for-best-players.png"

set title "Max Fitness, pop: 50, gen: 50, selection: 35%, mutation: 50%, mutationFactor: 20"
set key outside
set xlabel "generations"
set xrange [1:50]
set ylabel "games played"
set yrange [0:100]
# plot "info/defenseMaxCount.txt" u 3:2 title "best defense player" with lines lw 2 smooth csplines,\
#      "info/offenseMaxCount.txt" u 3:2 title "best offense player" with lines lw 2 smooth csplines
plot "info/offenseMaxCount.txt" u 3:2 title "best offense player" with lines lw 2 smooth csplines