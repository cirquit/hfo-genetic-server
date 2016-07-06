set term pngcairo size 1366,768 enhanced font "Verdana"
set output "current/games-played-for-best-players.png"

set title "Max Fitness, pop: 20, gen: 20, selection: 45%, mutation: 75%, mutationFactor: 30"
set key outside
set xlabel "generations"
set xrange [1:50]
set ylabel "games played"
set yrange [0:100]
# plot "info/defenseMaxCount.txt" u 3:2 title "best defense player" with lines lw 2 smooth csplines,\
#      "info/offenseMaxCount.txt" u 3:2 title "best offense player" with lines lw 2 smooth csplines
plot "info/offenseMaxCount.txt" u 3:2 title "best offense player" with lines lw 2 smooth csplines