#! /bin/bash

# this script allows you to brute force find the optimal shear stress values for Greenland and Antarctica by
# comparing the true thickness to the reconstructed thickness. Note, make sure that the run.sh script is
# set to just calculate time "0", or you will waste time!

for loops in $(seq 1 1)
do

	echo "loop #: ${loops}"
	bash run.sh
      bash run_parallel.sh
	bash adjust_shear_stress.sh

done
