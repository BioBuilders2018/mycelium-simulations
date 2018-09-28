#!/bin/sh
echo "hi"
# letters
declare -a arr=("H" "Y" "P" "A" "E" "C" "K" "R" "S")

declare -a a="ani/Animate_"

# loop through letters
for i in "${arr[@]}"
do
	echo "$i"
	convert -delay 12 -loop 0 $a$i*.png  ani/Simulation_$i.gif
	rm $a$i*.png
done
