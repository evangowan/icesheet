#! /bin/bash

time=$(awk '{if (NR == 1) print $0}' run_parameters)
region=$(awk '{if (NR == 2) print $0}' run_parameters)
run_number=$(awk '{if (NR == 3) print $0}' run_parameters)
run_description=$(awk '{if (NR == 4) print $0}' run_parameters)
earth_model=$(awk '{if (NR == 5) print $0}' run_parameters)
North_America_run_number=$(awk '{if (NR == 6) print $0}' run_parameters)
Eurasia_run_number=$(awk '{if (NR == 7) print $0}' run_parameters)
Antarctica_run_number=$(awk '{if (NR == 8) print $0}' run_parameters)
icesheet_spacing=$(awk '{if (NR == 9) print $0}' run_parameters)
icesheet_interval=$(awk '{if (NR == 10) print $0}' run_parameters)
latitude_spacing=$(awk '{if (NR == 11) print $0}' run_parameters)
longitude_spacing=$(awk '{if (NR == 12) print $0}' run_parameters)
your_name=$(awk '{if (NR == 15) print $0}' run_parameters)
gia_deformation=$(awk '{if (NR == 16) print $0}' run_parameters)
adjust_file=$(awk '{if (NR == 17) print $0}' run_parameters)

if [ ! -e "${region}/plots/" ]
then

	mkdir ${region}/plots/

fi

storage_folder=${region}/plots/${your_name}_${run_number}
mkdir ${storage_folder}

mkdir ${storage_folder}/deform
mkdir ${storage_folder}/topo
mkdir ${storage_folder}/thickness
mkdir ${storage_folder}/shear_stress

for time in $(cat times_to_calculate)
do

	if [ -e "run/${time}/deform.nc" ]
	then
		cp run/${time}/plots/deformed_base_topo.ps ${storage_folder}/deform/${time}.ps
		cp run/${time}/deform.nc ${storage_folder}/deform/${time}.nc
	fi

	cp run/${time}/plots/ice_elevation.ps ${storage_folder}/topo/${time}.ps	
	cp run/${time}/ice_topo.nc ${storage_folder}/topo/${time}.nc	
	cp run/${time}/plots/ice_thickness.ps ${storage_folder}/thickness/${time}.ps	
	cp run/${time}/ice_thickness.nc ${storage_folder}/thickness/${time}.nc
	cp run/${time}/plots/shear_stress.ps ${storage_folder}/shear_stress/${time}.ps	

done
