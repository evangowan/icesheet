#! /bin/bash

# setup for global ice sheet reconstructions

####################################
# Times to calculate
####################################

interval=2500
max_time=80000

number_times=$( echo "${max_time} / ${interval} + 1" | bc )

# assuming time zero will take the least amount of time, so starting from the oldest time should be most computationally efficient with multiple processors

seq ${max_time} -${interval} 0 > times_to_calculate

# if instead you just want to calculate specific times, use this instead

#cat << END > times_to_calculate
#20000
#END



####################################
# Region
####################################

# for sanity sake, I'm setting it up to do one region at a time, and combine them afterwards

#region=North_America
#region=Eurasia
region=Antarctica
#region=Patagonia

# information that will be put into the file ${region}/run_info.txt
# !!!!!!!! very important !!!!!!!!!, change this number for every run, the run number will be used to identify a GIA deformation run!
# also include your name, this will make it possible to distinguish run numbers from different authors
run_number="46" 
your_name="Evan" # no spaces or underscores!
run_description="Second Antarctica run for 80000 years, without GIA"

folder_on="false"

# For the creation of the SELEN input file, indicate the run number of ice sheet that you want to use, since it is currently set up to calculate
# one ice sheet at a time (e.g. the SELEN file must be created each time you calculate one ice sheet, otherwise the fragment file is not created!)
# Just run "selen_format.sh" after every icesheet run!

selen_North_America_run_number=19
selen_Eurasia_run_number=15
selen_Antarctica_run_number=18

# the above isn't currently used since I am now directly creating the Tegmark grid separately

####################################
# Earth Model
####################################

# all Earth models in this script have a lithosphere, upper mantle and lower mantle. The values have
# a four character setup that is described in the file earth_model_format_codes.txt

# model ehgk is what I used for NAICE (Gowan et al 2016), probably not appropriate for whole earth. 
# West Antarctica should have a lower upper mantle value, for instance.
lithosphere=h
upper_mantle=g
lower_mantle=G

earth_model=e${lithosphere}${upper_mantle}${lower_mantle}

# the ice models used for calculating GIA
# "I" stands for ICE6G
North_America_run_number=44
Eurasia_run_number=43
Antarctica_run_number=42
Patagonia_run_number=45

# if instead you want present day topography, set ${earth_model} to null
#earth_model="null" 



gia_deformation=icesheet_${your_name}_${earth_model}_${North_America_run_number}_${Eurasia_run_number}_${Antarctica_run_number}_${Patagonia_run_number}.dat

####################################
# Resolution parameters
####################################

# input parameters for ICESHEET:

icesheet_spacing=5 # in km, minimum distance between flowlines in ICESHEET calculation, also used for the topography and shear stress grids
icesheet_interval=20 # in m, contour interval used in ICESHEET

# resolution for the final model, for later input into SELEN

latitude_spacing=0.25 # in degrees
longitude_spacing=0.50 # in degress


##################################
##################################

# with all of that above, all the user editable parameters have been set, the stuff below just runs the
# scripts to create the basal topography binary file, run icesheet, and merge it into a file for input into SELEN

# check if run exists, if so, it will terminate before doing anything. This folder is only created when the full ice sheet model has finished.

reconstruction_id=${your_name}_${run_number}

if [ -d ${region}/reconstructions/${reconstruction_id} ]
then
  echo "reconstruction number value already exists, go into run.sh and change this!"
  echo "terminating run..."
  exit 0
fi


if [ ! -d "run" ]
then
	mkdir run
fi

chmod 777 run

cat <<END_CAT > run_parameters
${max_time}
${region}
${run_number}
${run_description}
${earth_model}
${North_America_run_number}
${Eurasia_run_number}
${Antarctica_run_number}
${icesheet_spacing}
${icesheet_interval}
${latitude_spacing}
${longitude_spacing}
${interval}
${number_times}
${your_name}
${gia_deformation}
adjust_0.txt
${folder_on}
${selen_North_America_run_number}
${selen_Eurasia_run_number}
${selen_Antarctica_run_number}
END_CAT

echo "------------------------------------------------"  >> ${region}/log_file.txt
cat run_parameters >> ${region}/log_file.txt


for time in $(cat times_to_calculate)
do

	if [ -d "run/${time}" ]
	then
		rm -R run/${time}
	fi

  	mkdir run/${time}
	chmod 777 run/${time}
	# put all the parameters into a file that will be read in the next script

	cat <<END_CAT > run/${time}/run_parameters
${time}
${region}
${run_number}
${run_description}
${earth_model}
${North_America_run_number}
${Eurasia_run_number}
${Antarctica_run_number}
${icesheet_spacing}
${icesheet_interval}
${latitude_spacing}
${longitude_spacing}
${interval}
${number_times}
${your_name}
${gia_deformation}
adjust_0.txt
${folder_on}
${selen_North_America_run_number}
${selen_Eurasia_run_number}
${selen_Antarctica_run_number}
END_CAT

	cp prepare_icesheet.sh run/${time}
	chmod 777 run/${time}/prepare_icesheet.sh

done



