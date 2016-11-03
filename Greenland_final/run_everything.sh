#! /bin/bash

##################################################
#                                                #
#        Parameters user can change              #
#                                                #
##################################################

##############################################
#    parameters related to input grids       #
##############################################

# The process of extracting the outline of the Greenland ice sheet takes a while, so only set this to "y" 
# the first time you run the script. Set to "n" afterwards.

execute_outline="y"

# if you haven't made a change to the topography (i.e. after a GIA step) you can turn off the shear stress file calculation.
# It doesn't really take a huge amount of time, so you can leave it on if you want.

execute_ss="y"


# resolution used for the topography and shear stress grids. In meters.

grid_resolution=5000



#####################################################
#   parameters related to ICESHEET reconstructions  #
#####################################################


# the following two parameters control the detail of the ICESHEET reconstructions.

# 1) Elevation contour interval used in ICESHEET, in meters. Recommended value for production runs is 20 m.

elevation_contour_interval=20 # in m

# 2) Minimum distance along the contour line where flowlines are calculated in ICESHEET. 
#    Suggested value for production runs is 5 km (if using a grid resolution of 5000 m)

initiation_distance_km=20



# the following three parameters do not have any numerical effect on the simulation, they are just used for file naming

# 1) value for the time period used in this script

time_period=0

# 2) Value for the current GIA iteration (not done in this Greenland example)

gia_iteration=0

# 3) value to indicate which shear stress model you are using

shear_stress=0


# The first time you run this, set to "y", so that it computes the averaged true Greenland ice thickness. 
# Set to "n" after, it takes a lot of time to run!
execute_surface_mean="y"




##############################################
# check executables and run scripts. It 
# shouldn't be necessary to change anything 
# below here.
##############################################



# check if GMT is installed
gmt_exists=$(command -v grdcontour)


if [ -z "${gmt_exists}" ];
then

	echo "before running this script, you need to install Generic Mapping Tools,"
	echo "and make sure it is visible in \${path}"

	exit 0
fi

# check if NetCDF tools are installed
nco_exists=$(command -v ncdump)


if [ -z "${gmt_exists}" ];
then

	echo "before running this script, you need to install NetCDF Tools,"
	echo "and make sure it is visible in \${path}"

	exit 0
fi


# check if icesheet is compiled, if not, it will compile it

icesheet_exists=$(command -v icesheet)

if [ -z "${icesheet_exists}" ];
then

	binary_path="../"

	icesheet_exists2=$(command -v ../icesheet)


	if [ -z "${icesheet_exists2}" ]; # need to compile everything
	then

		cd ../
		make icesheet
		make nearest_int
		make reduce_dem

		cd Greenland_final

	fi
	
else

	binary_path=""

fi

# run create shear_stress_file.sh


cd shear_stress

bash create_shear_stress_file.sh

cd ..

# run convert_grid

# make sure you change the path to the location of the Greenland dataset
# download from here: https://nsidc.org/data/IDBMG4/versions/2
greenland_netcdf=MCdataset-2015-04-27.nc

	if [ -z "${greenland_netcdf}" ]; # need to download Greenland data
	then

		echo "you need to download the Greenland data: ${greenland_netcdf}"
		echo "and put it in this directory"
		echo "https://nsidc.org/data/IDBMG4/versions/2"
		exit 0

	fi


# run convert_grid.sh



bash convert_grid.sh ${execute_outline} ${execute_ss} ${grid_resolution} ${binary_path}


bash run.sh ${elevation_contour_interval} ${initiation_distance_km} ${time_period} ${gia_iteration} ${shear_stress} ${execute_surface_mean} ${binary_path}


