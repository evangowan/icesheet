#! /bin/bash

# must have GNU Parallel installed

# also must run "run.sh" to generate the folders

number_processors=3

doit() {
	time_var=$1

	time_folder=run/${time_var}
	cd ${time_folder}
	bash prepare_icesheet.sh
	cd ../..
}

export -f doit
cat times_to_calculate | parallel  --jobs ${number_processors} doit 
