#!/bin/sh
echo "hi"
# letters
declare -a arr=("H" "Y" "P" "A" "E" "C" "K" "R" "S")
declare -a a="fig/figmate_"

# loop through letters
for i in "${arr[@]}"
do
	echo "$i"
	convert -delay 12 -loop 0 $a$i*.png  fig/Simulation_$i.gif
	rm $a$i*.png
done

# simulation plots
convert fig/figmate_hyphal_*.png -delay 12 -loop 0 fig/Simulation_hyphal.gif
convert fig/figmate_substrate_*.png -delay 12 -loop 0 fig/Simulation_substrate.gif
convert fig/figmate_density_*.png -delay 12 -loop 0 fig/Simulation_density.gif

rm *.png
