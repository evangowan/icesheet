#! /bin/bash

# it is expected this script will run after using SELEN and before starting a new ICESHEET run (so the run_parameters file is not overwritten)

max_time=$(awk '{if (NR == 1) print $0}' run_parameters)

run_number=$(awk '{if (NR == 3) print $0}' run_parameters)
run_description=$(awk '{if (NR == 4) print $0}' run_parameters)


Eurasia_run_number=$(awk '{if (NR == 7) print $0}' run_parameters)
Antarctica_run_number=$(awk '{if (NR == 8) print $0}' run_parameters)
icesheet_spacing=$(awk '{if (NR == 9) print $0}' run_parameters)
icesheet_interval=$(awk '{if (NR == 10) print $0}' run_parameters)
latitude_spacing=$(awk '{if (NR == 11) print $0}' run_parameters)
longitude_spacing=$(awk '{if (NR == 12) print $0}' run_parameters)
interval=$(awk '{if (NR == 13) print $0}' run_parameters)
number_times=$(awk '{if (NR == 14) print $0}' run_parameters)
your_name=$(awk '{if (NR == 15) print $0}' run_parameters)


# region calculated from SELEN

region="North_America"
North_America_run_number=${run_number}

#Earth model used to calculate deformation

earth_model=ehgk

selen_output="/scratch/users/egowan-local/gia/SELEN-forked/selen/DEPOTS/depot-TEST/rsl/rsl-contours/rsl_spreadsheet.dat"

cp ${selen_output} ${region}/deform/icesheet_${your_name}_${earth_model}_${North_America_run_number}_${Eurasia_run_number}_${Antarctica_run_number}.dat


