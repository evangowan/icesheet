#! /bin/bash

# make sure that it is in the correct order

storage_location="gia/results"

your_name=Evan

# Ice sheet runs used to calculate GIA
North_America_run_number=39
Eurasia_run_number=35
Antarctica_run_number=36
Patagonia_run_number=37


#Earth model used to calculate deformation

earth_model=ehgG

selen_output="${storage_location}/${your_name}_${earth_model}_${North_America_run_number}_${Eurasia_run_number}_${Antarctica_run_number}_${Patagonia_run_number}/rsl_grid.dat"

cp ~/${selen_output} deform/icesheet_${your_name}_${earth_model}_${North_America_run_number}_${Eurasia_run_number}_${Antarctica_run_number}_${Patagonia_run_number}.dat


