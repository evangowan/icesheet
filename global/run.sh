#! /bin/bash

# setup for global ice sheet reconstructions

####################################
# Region
####################################

# for sanity sake, I'm setting it up to do one region at a time, and combine them afterwards

region=North_America
#region=Eurasia
#region=Antarctica
#region=Iceland

# information that will be put into the file ${region}/run_info.txt
# !!!!!!!! very important !!!!!!!!!, change this number for every run, the run number will be used to identify a GIA deformation run!
# also include your name, this will make it possible to distinguish run numbers from different authors
run_number="1" 
your_name="Evan" # no spaces or underscores!
run_description="Initial model with null topography"



####################################
# Earth Model
####################################

# all Earth models in this script have a lithosphere, upper mantle and lower mantle. The values have
# a four character setup that is described in the file earth_model_format_codes.txt

# model ehgk is what I used for NAICE (Gowan et al 2016), probably not appropriate for whole earth. 
# West Antarctica should have a lower upper mantle value, for instance.
lithosphere=h
upper_mantle=g
lower_mantle=k

earth_model=e${lithosphere}${upper_mantle}${lower_mantle}

# the ice models used for calculating GIA, set to zero if they were not used 
North_America_run_number=0
Eurasia_run_number=0
Antarctica_run_number=0
Iceland_run_number=0

# if instead you want present day topography, set ${earth_model} to null
earth_model="null" 

#gia_deformation=${your_name}_${earth_model}_${North_America_run_number}_${Eurasia_run_number}_${Antarctica_run_number}_${Iceland_run_number}

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

for time in 12000
do

	if [ -d "run/${time}" ]
	then
		rm -R run/${time}
	fi

  	mkdir run/${time}
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
END_CAT

	cp prepare_icesheet.sh run/${time}


done



