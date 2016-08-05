set term pngcairo size 1366,768 enhanced font "Verdana"
set output "current/catch.png"

set title "Catch Action"
set palette defined (0 1 1 1, 1 0 0 0)
set xrange [-0.5:3.5]
set yrange [-0.5:3.5]

set cbrange [0:100]
set cblabel "P(Action)"

unset key
unset xtic
unset ytic

plot "info/catch.dat" matrix with image ,\
     "info/catch.dat" matrix using 1:2:(sprintf("%g",$3)) with labels
