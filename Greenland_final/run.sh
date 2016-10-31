#! /bin/bash

# run convert_grid.sh before attempting to run this script

# simulation id

elevation_contour_interval=20 # in m
initiation_distance_km=15
time_period=20000
gia_iteration=0

shear_stress=0

for initiation_distance_km in 20 #5 #1 3 5 10 15 20 30 40 50 100
do

for elevation_contour_interval in 20 #10 20 30 40 50
do




simulation=${initiation_distance_km}k_${elevation_contour_interval}m_${time_period}a_gia${gia_iteration}_mostlikely_${shear_stress}

path_to_icesheet="../"

ice_margin="outline.xyz"
elevation_parameter_file="elev_parameters.txt"
shear_stress_parameter_file="ss_parameters.txt"

initiation_distance=${initiation_distance_km}000

cat << END_params > params.txt
${ice_margin}
${elevation_parameter_file}
${shear_stress_parameter_file}
${elevation_contour_interval}
${initiation_distance}
END_params

mkdir results
mkdir plots
echo ${simulation}  > results/${simulation}
echo "" >> results/${simulation}
cat params.txt >> results/${simulation}

echo "" >> results/${simulation}
echo "Start time" >>  results/${simulation}
date >>  results/${simulation}
echo "" >> results/${simulation}

${path_to_icesheet}icesheet

echo "" >> results/${simulation}
echo "End time" >>  results/${simulation}
date >>  results/${simulation}
echo "" >> results/${simulation}


awk '{if ($1 != ">") print $0}' contours.txt > results/${simulation}_results.txt

bash plot2.sh ${simulation}

done
done
