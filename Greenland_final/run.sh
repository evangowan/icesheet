#! /bin/bash

# run convert_grid.sh before attempting to run this script


# path to the binaries of the ICESHEET program. This script assumes that the compiled programs are in the base directory (i.e. one up from where this file is). If you have put the binaries somewhere else (e.g. ${PATH}) change the variable below
path_to_icesheet="../"

# simulation id

${elevation_contour_interval} ${initiation_distance_km} ${time_period} ${gia_iteration} ${shear_stress}

elevation_contour_interval=$1 # in m
initiation_distance_km=$2
time_period=$3
gia_iteration=$4
shear_stress=$5

execute_surface_mean=$6

# set default values if they are not given by command line

if [ -z "${elevation_contour_interval}" ]
then

	elevation_contour_interval=20

fi


if [ -z "${initiation_distance_km}" ]
then

	initiation_distance_km=5

fi


if [ -z "${time_period}" ]
then

	time_period=0

fi

if [ -z "${gia_iteration}" ]
then

	gia_iteration=0

fi

if [ -z "${shear_stress}" ]
then

	shear_stress=0

fi

if [ -z "${execute_surface_mean}" ]
then

	execute_surface_mean=y

fi


binary_path=$7 


icesheet_exists2=$(command -v ${binary_path}icesheet)


if [ -z "${icesheet_exists2}" ]; 
then

	if [ -z ${binary_path} ]
	then
		binary_path="../"
	else

		binary_path=""

	fi	

fi

icesheet_exists2=$(command -v ${binary_path}icesheet)

if [ -z "${icesheet_exists2}" ]; 
then

	echo "compile icesheet before running this script"

fi


# for batch runs, you can uncomment these lines and use as many values as you want


#for initiation_distance_km in 20 #5 #1 3 5 10 15 20 30 40 50 100
#do

#for elevation_contour_interval in 20 #10 20 30 40 50
#do




simulation=${initiation_distance_km}k_${elevation_contour_interval}m_${time_period}a_gia${gia_iteration}_mostlikely_${shear_stress}


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

bash plot2.sh ${simulation} ${execute_surface_mean}

#done
#done
