#! /bin/bash

# run ICESHEET serially



# line in the file "times_to_calculate"
run_number=12

time_var=$(awk -v line_number=${run_number} '{if (NR == line_number) print $1}' times_to_calculate)

time_folder=run/${time_var}
cd ${time_folder}
bash prepare_icesheet.sh
cd ../..
