#!/bin/gnuplot
# Generate frames from history.

set terminal png truecolor size 500, 300

FILE = 'sim.txt'
stats FILE index 1 using (max_height = $1) nooutput
stats FILE index 2 name "sim" nooutput
array iter[sim_records]
array choice[sim_records]
stats FILE index 2 using (iter[$0+1] = $1) nooutput
stats FILE index 2 using (choice[$0+1] = $2) nooutput

nbamboos = sim_columns - 2
array rates[nbamboos]
do for [i=1:nbamboos] {
  stats FILE index 0 using (column(i)) nooutput
  rates[i] = STATS_min
}
rates_str = ""
do for [i=1:nbamboos] {rates_str = rates_str.sprintf(" %.5f", rates[i])}

set xrange [(-0.02 * (nbamboos - 1)):((nbamboos - 1) * 1.02)]
set yrange [0:(max_height * 1.02)]
set grid ytics
set xtics 1
unset key

do for [it = 1:sim_records] {
unset label
set output sprintf("frames/%05.0f.png", iter[it])

## TODO: Show the rate on top of each current height, to show what the future
##       bamboo height will be. These can be two superposed impulses, one with
##       height hi, the other with hi + rate. Maybe put heights in arrays in
##       order to be able to perform sums.

set title sprintf("Rates:%s; it = %d", rates_str, iter[it])
set label 1 "↓" at choice[it],(max_height*1.01) font ",24" center
plot for [i=3:sim_columns] FILE index 2 every ::(it-1)::(it-1) \
     using (i-3):(column(i) + rates[i-2]) with impulses lc 2 lw 3, \
     for [i=3:sim_columns] FILE index 2 every ::(it-1)::(it-1) \
     using (i-3):(column(i)) with impulses lc 1 lw 3
}
