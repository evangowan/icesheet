#! /bin/bash

########################################################################################################################
# Before running this script, you need to change the paths where the ice thickness and deformation files are
########################################################################################################################


# this assumes that you have extracted the deformation files in ../deformation

deformation_1=../deformation/icesheet_Evan_ehgr_71_67_64_65.dat
deformation_p25=../deformation/icesheet_Evan_ehgr_71_67_64_65_quarter.dat

resolution_values="1 0.25"

resolution_1_files=""
resolution_p25_files=""

source ../ice_reconstruction_files/North_America/projection_info.sh


if [ ! -d "no_Canada_ice" ]
then
   mkdir no_Canada_ice
fi

# creates base topography without Canadian ice caps. Data from NASA ICEBRIDGE radar data

grdmath Rtopo-2/filtered_bed_topo_0.25.nc Canada_ice_thickness/Canadian_Ice_Thickness_0.25.nc SUB = no_Canada_ice/topo_0.25.nc
grdmath Rtopo-2/filtered_bed_topo_1.nc Canada_ice_thickness/Canadian_Ice_Thickness_1.nc SUB = no_Canada_ice/topo_1.nc

if [ ! -d "temp" ]
then
   mkdir temp
fi

rm temp/*


for time_slice in 0 2500 5000 7500 10000 12500 15000 17500 20000 22500 25000 27500 30000 32500 35000 37500 40000 42500 45000 47500 50000 52500 55000 57500 60000 62500 65000 67500 70000 72500 75000 77500 80000
do




	# extract ice thickness

	for region in North_America Eurasia Patagonia  Antarctica 
	do
		special_projection="n"
		source ../ice_reconstruction_files/${region}/projection_info.sh
		grd2xyz ../ice_reconstruction_files/${region}/thickness/${time_slice}.nc  > temp/projected_raw.txt

		
		echo ${special_projection}

		for resolution in  ${resolution_values}
		do

			if [ "${resolution}" = "1" ]
			then
				filter_width=100000
			else
				filter_width=25000
			fi

			grdfilter ../ice_reconstruction_files/${region}/thickness/${time_slice}.nc   -D0 -Fg${filter_width} -Gtemp/filtered.nc

			if [ "${special_projection}" = "y" ]
			then
			echo "should be using new projected info"
			# because Antarctica is no fun

				grdproject temp/filtered.nc ${R_options} ${J_options_project} -C -F -I -D${resolution}  -Gtemp/ice_thickness_temp.nc

			else
			echo "should not be using new projected info"



				if [ "${region}" = "North_America" ]
				then
					R_options_temp="-R-135/25/0/90r"
					grdproject temp/filtered.nc ${R_options_temp} ${J_options} -F -I -D${resolution} -Gtemp/ice_thickness_temp.nc 
				else
					grdproject temp/filtered.nc ${R_options} ${J_options} -F -I -D${resolution} -Gtemp/ice_thickness_temp.nc

				fi


			fi

			grd2xyz temp/ice_thickness_temp.nc   > temp/ice_thickness_temp.txt

			xyz2grd temp/ice_thickness_temp.txt -Gtemp/full_temp.nc  -Rd -I${resolution} 

			grdmath temp/full_temp.nc 0 DENAN = temp/ice_thickness_${region}_${resolution}.nc


		

		done

	done



	for resolution in ${resolution_values}
	do
		grdmath temp/ice_thickness_North_America_${resolution}.nc temp/ice_thickness_Eurasia_${resolution}.nc ADD  temp/ice_thickness_Antarctica_${resolution}.nc ADD temp/ice_thickness_Patagonia_${resolution}.nc ADD =  temp/ice_thickness_${resolution}.nc
		if [ "${resolution}" = "1" ]
		then
			awk -v time=${time_slice} '{if($1*1000 == time) print $2, $3, $4}' ${deformation_1} > temp/gia.txt
			resolution_1_files="${resolution_1_files} temp/${time_slice}_${resolution}.nc"
		elif  [ "${resolution}" = "0.25" ]
		then
			awk -v time=${time_slice} '{if($1*1000 == time) print $2, $3, $4}' ${deformation_p25} > temp/gia.txt
			resolution_p25_files="${resolution_p25_files} temp/${time_slice}_${resolution}.nc"
		else
			echo "wrong resolution"
		fi

		xyz2grd temp/gia.txt -Rd -I${resolution} -Gtemp/deformation_${resolution}.nc # gives this a proper geographical NetCDF header
		grdmath no_Canada_ice/topo_${resolution}.nc temp/deformation_${resolution}.nc SUB = temp/deform_base_topo_${resolution}.nc
		grdmath temp/ice_thickness_${resolution}.nc temp/deform_base_topo_${resolution}.nc ADD = temp/paleo_topography_${time_slice}_${resolution}.nc

		# the netcdf files have been created, now to merge them

		z_name=sea_level
		z_long_name="calculated sea level excluding topography"

		deformation=temp/deformation_${resolution}.nc
		deform_base_topo=temp/deform_base_topo_${resolution}.nc
		ice_thickness=temp/ice_thickness_${resolution}.nc
		paleo_topography=temp/paleo_topography_${time_slice}_${resolution}.nc


		ncap2 -O -s "defdim(\"time\",1);time[time]=-${time_slice}.0;time@long_name=\"Time\";time@units=\"years since 1950-01-1;\";time@calendar=\"365_day\"" ${deformation} temp/${z_name}_time.nc
		ncecat -O -u time  temp/${z_name}_time.nc  temp/${z_name}_time2.nc
		ncrename -O -v z,${z_name}  temp/${z_name}_time2.nc
		ncatted -O -a units,${z_name},o,c,"m" temp/${z_name}_time2.nc
		ncatted -O -a long_name,${z_name},o,c,"${z_long_name}" temp/${z_name}_time2.nc

		z_name=base_topography
		z_long_name="deformed base topography excluding ice sheets"



		ncap2 -O -s "defdim(\"time\",1);time[time]=-${time_slice}.0;time@long_name=\"Time\";time@units=\"years since 1950-01-1;\";time@calendar=\"365_day\"" ${deform_base_topo} temp/${z_name}_time.nc
		ncecat -O -u time  temp/${z_name}_time.nc  temp/${z_name}_time2.nc
		ncrename -O -v z,${z_name}  temp/${z_name}_time2.nc
		ncatted -O -a units,${z_name},o,c,"m" temp/${z_name}_time2.nc
		ncatted -O -a long_name,${z_name},o,c,"${z_long_name}" temp/${z_name}_time2.nc


		z_name=ice_thickness
		z_long_name="grounded ice thickness"


		ncap2 -O -s "defdim(\"time\",1);time[time]=-${time_slice}.0;time@long_name=\"Time\";time@units=\"years since 1950-01-1;\";time@calendar=\"365_day\"" ${ice_thickness} temp/${z_name}_time.nc
		ncecat -O -u time  temp/${z_name}_time.nc  temp/${z_name}_time2.nc
		ncrename -O -v z,${z_name}  temp/${z_name}_time2.nc
		ncatted -O -a units,${z_name},o,c,"m" temp/${z_name}_time2.nc
		ncatted -O -a long_name,${z_name},o,c,"${z_long_name}" temp/${z_name}_time2.nc



		z_name=paleo_topography
		z_long_name="topography accounting for ice thickness, sea level change and deformation"


		ncap2 -O -s "defdim(\"time\",1);time[time]=-${time_slice}.0;time@long_name=\"Time\";time@units=\"years since 1950-01-1;\";time@calendar=\"365_day\"" ${paleo_topography} temp/${z_name}_time.nc
		ncecat -O -u time  temp/${z_name}_time.nc  temp/${z_name}_time2.nc
		ncrename -O -v z,${z_name}  temp/${z_name}_time2.nc
		ncatted -O -a units,${z_name},o,c,"m" temp/${z_name}_time2.nc
		ncatted -O -a long_name,${z_name},o,c,"${z_long_name}" temp/${z_name}_time2.nc


		cdo -O merge  temp/paleo_topography_time2.nc temp/sea_level_time2.nc  temp/base_topography_time2.nc  temp/ice_thickness_time2.nc temp/${time_slice}_${resolution}.nc

	done



done



echo ${resolution_1_files}
echo ${resolution_p25_files}
cdo -O mergetime  ${resolution_1_files} reconstruction_1_degree.nc
cdo -O mergetime  ${resolution_p25_files} reconstruction_0.25_degree.nc

exit 0

source ../ice_reconstruction_files/North_America/projection_info.sh
makecpt -Cglobe -T-10000/10000 > temp/shades.cpt
grdimage temp/paleo_topography_20000_1.nc ${shift_up}  ${R_options} ${J_options}  -K -P -Ctemp/shades.cpt -V -nb > temp/plot.ps
pscoast -Bafg -O -K ${R_options} ${J_options} -P -Wthin -Di -A5000 -Wthin,black >> temp/plot.ps

exit 0

grdimage temp/paleo_topography_20000_0.25.nc ${shift_up}  ${R_options} ${J_options}  -K -P -Ctemp/shades.cpt -V -nb > temp/plot2.ps
pscoast -Bafg -O -K ${R_options} ${J_options} -P -Wthin -Di -A5000 -Wthin,black >> temp/plot2.ps

