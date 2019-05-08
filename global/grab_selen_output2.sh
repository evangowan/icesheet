#! /bin/bash

# make sure that it is in the correct order

storage_location="gia/results"

your_name=Evan

# Ice sheet runs used to calculate GIA
North_America_run_number=23
Eurasia_run_number=24
Antarctica_run_number=22
Patagonia_run_number=25


#Earth model used to calculate deformation

earth_model=ehgG

selen_output="${storage_location}/${your_name}_${earth_model}_${North_America_run_number}_${Eurasia_run_number}_${Antarctica_run_number}_${Patagonia_run_number}/rsl_grid.dat"

cp ~/${selen_output} deform/icesheet_${your_name}_${earth_model}_${North_America_run_number}_${Eurasia_run_number}_${Antarctica_run_number}_${Patagonia_run_number}.dat


