#! /bin/bash

# it is expected this script will run after using SELEN and before starting a new ICESHEET run (so the run_parameters file is not overwritten)

max_time=$(awk '{if (NR == 1) print $0}' run_parameters)

run_number=$(awk '{if (NR == 3) print $0}' run_parameters)
run_description=$(awk '{if (NR == 4) print $0}' run_parameters)

North_America_run_number=$(awk '{if (NR == 6) print $0}' run_parameters)
Eurasia_run_number=$(awk '{if (NR == 7) print $0}' run_parameters)
Antarctica_run_number=$(awk '{if (NR == 8) print $0}' run_parameters)
icesheet_spacing=$(awk '{if (NR == 9) print $0}' run_parameters)
icesheet_interval=$(awk '{if (NR == 10) print $0}' run_parameters)
latitude_spacing=$(awk '{if (NR == 11) print $0}' run_parameters)
longitude_spacing=$(awk '{if (NR == 12) print $0}' run_parameters)
interval=$(awk '{if (NR == 13) print $0}' run_parameters)
number_times=$(awk '{if (NR == 14) print $0}' run_parameters)
your_name=$(awk '{if (NR == 15) print $0}' run_parameters)

selen_North_America_run_number=$(awk '{if (NR == 19) print $0}' run_parameters)
selen_Eurasia_run_number=$(awk '{if (NR == 20) print $0}' run_parameters)
selen_Antarctica_run_number=$(awk '{if (NR == 21) print $0}' run_parameters)

# region calculated from SELEN

#region="North_America"
#North_America_run_number=${run_number}

#region="Eurasia"
#Eurasia_run_number=${run_number}

region="Antarctica"
Antarctica_run_number=${run_number}


#Earth model used to calculate deformation

earth_model=ehgk

selen_output="/scratch/users/egowan-local/gia/SELEN-forked/selen/sea_level/rsl_spreadsheet.dat"

cp ${selen_output} deform/icesheet_${your_name}_${earth_model}_${selen_North_America_run_number}_${selen_Eurasia_run_number}_${selen_Antarctica_run_number}.dat


