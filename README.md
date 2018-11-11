
This directory contains the code for hyphal growth simulation at the microscopic level as part of [DTU Biobuilders project for iGEM 2018](http://2018.igem.org/Team:DTU-Denmark/GrowthModelling).
# Hyphal growth simulations <img src="http://2018.igem.org/wiki/images/c/c9/T--DTU-Denmark--menu-icon-logo.png" align="right" alt="Hyphae hackers logo" />
Mycelium growth originates from a single spore, where branching starts. Here we follow the development of the mycelium by simulating branch development during a specified time period. We focus on simulating hyphal movement through a space with homogenous substrate, hyphal tip extension and branching events and how the mycelium density/biomass changes over time.

When the simulation is completed, several plots are produced. This includes summary plots of mycelium properties, an animation of the mycelium development and a density inspection.

## Running the simulation
The main code is located in `simulation_v7.R`, so run that. Libraries required are:
* [stats](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html)
* [scales](https://cran.r-project.org/web/packages/scales/index.html)
* [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/)

## Animated letters
For the fun of it, we can also simulate the letters in our team name for this year. The code for this exist in `letter_simulations.R` and the functions for each letter are defined in `letter_funcs.R`.

## Papers referred to in the code
Lejeune, R., Nielsen, J. og Baron, G. V. (1995) “Morphology of Trichoderma reesei QM 9414 in submerged cultures”, Biotechnology and Bioengineering, 47(5), s. 609–615. doi: 10.1002/bit.260470513.

Lejeune, R. og Baron, G. V. (1996) “Simulation of growth of a filamentous fungus in 3 dimensions.”, Biotechnology and bioengineering, 53(2), s. 139–50. doi: 10.1002/(SICI)1097-0290(19970120)53:2<139::AID-BIT3>3.0.CO;2-P.

Spohr, A., Dam-Mikkelsen, C., Carlsen, M., Nielsen, J. og Villadsen, J. (1998) “On-line study of fungal morphology during submerged growth in a small flow-through cell”, Biotechnology and Bioengineering, 58(5), s. 541–553. doi: 10.1002/(SICI)1097-0290(19980605)58:5<541::AID-BIT11>3.0.CO;2-E.
