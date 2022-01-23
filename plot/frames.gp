#!/bin/gnuplot
# Generate frames from history.

set terminal png truecolor size 500, 300

FILE = 'sim.txt'
stats FILE index 0 using (max_height = $1) nooutput
stats FILE index 1 name "sim" nooutput
array iter[sim_records]
array choice[sim_records]
stats FILE index 1 using (iter[$0+1] = $1) nooutput
stats FILE index 1 using (choice[$0+1] = $2) nooutput

set xrange [-0.2:(sim_columns - 3 + 0.2)]
set yrange [0:(max_height + 0.1)]
set grid ytics
set xtics 1
unset key

do for [it = 1:sim_records] {
unset label
set output sprintf("frames/%05.0f.png", iter[it])

set title sprintf("i = %d", iter[it])
set label 1 "↓" at choice[it],max_height
plot for [i=3:sim_columns] FILE index 1 every ::(it-1)::(it-1) \
     using (i-3):(column(i)) with impulses lc 1 lw 3
}
