#! /bin/bash

# creates file for use in SELEN. Needs 

max_time=$(awk '{if (NR == 1) print $0}' run_parameters)
region=$(awk '{if (NR == 2) print $0}' run_parameters)
run_number=$(awk '{if (NR == 3) print $0}' run_parameters)
run_description=$(awk '{if (NR == 4) print $0}' run_parameters)
earth_model=$(awk '{if (NR == 5) print $0}' run_parameters)
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


latitude_spacing=0.5 # make sure to delete this later

max_time=20000 # make sure to delete this later

file_out=${region}/reconstructions/icesheet_${run_number}


# header information

cat << END_CAT > ${file_out}
${region}     -  region calculated
${your_name}    -  name of person who calculated this
${run_number}   - run number
${latitude_spacing}     -   Latitude spacing
${max_time}     -   Maximum time
${interval}     -  Time interval
icesheet_${your_name}_${earth_model}_${North_America_run_number}_${Eurasia_run_number}_${Antarctica_run_number}    - file used to calculate GIA deformation
END_CAT

# now the fun part, combine everything

source ${region}/projection_info.sh

mapproject << END    -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F  > corners.txt
${west_longitude} ${west_latitude}
${east_longitude} ${east_latitude}
END

mapproject corners.txt    -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F -I


spacing=${icesheet_spacing}000
coarse_spacing=40000 # just for plotting

r1=$(awk '{if (NR==1) print $1}' corners.txt)
r2=$(awk '{if (NR==2) print $1}' corners.txt)
r3=$(awk '{if (NR==1) print $2}' corners.txt)
r4=$(awk '{if (NR==2) print $2}' corners.txt)

# round the numbers, should only need to do this for the top left corner, really

x_min=${r1}
y_min=${r3}
x_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r2} / ${spacing}" | bc ) )
x_max=$(echo "${x_max_temp} * ${spacing}" | bc)
y_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r4} / ${spacing}" | bc ) )
y_max=$(echo "${y_max_temp} * ${spacing}" | bc)

mkdir temp

ocean_equivalent=${region}/topo/ocean_equivalent_ice.nc

grdmath ${ocean_equivalent} 0 GT = ocean_mask.nc

makecpt -Crainbow -T0/5000  > shades_ice.cpt

for times in 20000 15000 10000 6000 0 #$( seq ${max_time} -${interval} 0)
do

	# as of SELEN 2.8, there is no accounting for grounded ice, so you have to subtract that part of the load off
	# According to G. Spada, SELEN 4.0 will be able to take this into account

	grdmath run/${times}/ice_thickness.nc 0 GT = ice_mask.nc
	grdmath ice_mask.nc ocean_mask.nc MUL = ocean_ice_mask.nc

	grdmath run/${times}/ice_thickness.nc ${ocean_equivalent} SUB ocean_ice_mask.nc MUL = ice_ocean_region.nc

	grdmath ocean_ice_mask.nc 0 EQ run/${times}/ice_thickness.nc MUL ice_ocean_region.nc ADD = adjusted_ice_thickness.nc

	# account for Great Lakes if you are creating a North America grid. Will need to be added regardless of SELEN verison

	if [ "${region}" = "North_America" ]
	then
		grdmath North_America/topo/great_lakes_equivalent_ice_thickness.nc 0 GT = lake_mask.nc
		grdmath adjusted_ice_thickness.nc North_America/topo/great_lakes_equivalent_ice_thickness.nc LT lake_mask.nc MUL = add_lake_mask.nc
		grdmath add_lake_mask.nc North_America/topo/great_lakes_equivalent_ice_thickness.nc adjusted_ice_thickness.nc IFELSE = adjusted_ice_thickness_lakes.nc
		mv -f  adjusted_ice_thickness_lakes.nc adjusted_ice_thickness.nc
	fi

	

#	plot=test.ps



#grdimage adjusted_ice_thickness.nc -Y12  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r  -JX${map_width}/0 -K -P -Cshades_ice.cpt -V -nb > ${plot}

#pscoast -Bafg -O -K -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}
#psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx1000f500+l"Ice Thickness (m)" --FONT_LABEL=14p  -Cshades_ice.cpt -V  >> $plot

	# convert grid to geographical coordinate system

longmin=-180
longmax=0
latmin=30
latmax=85

	grd2xyz adjusted_ice_thickness.nc > temp.xyz
	mapproject temp.xyz -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -I -F  > temp_geo.xyz
	blockmean -Rg -I${latitude_spacing} temp_geo.xyz -V > bm.out

	surface bm.out -Gglobal.nc  -I${latitude_spacing} -Rg -T0.75  -V

	grdmask ${region}/margins/${times}.gmt -Gglobal_mask.nc -I${latitude_spacing} -Rg
	grdmath global.nc global_mask.nc MUL = ice_thickness_geo_regular.nc



#	grdproject adjusted_ice_thickness.nc   -JA${center_longitude}/${center_latitude}/${map_width} -I -Gice_thickness_geo.nc    -Fe  -V  
#
#	grdsample ice_thickness_geo.nc -Gice_thickness_geo_regular.nc -I${latitude_spacing}
#
#	grdmath ice_thickness_geo_regular.nc 0 DENAN = ice_thickness_geo_regular.nc

	grd2xyz ice_thickness_geo_regular.nc > temp/${times}.xyz

	if [ "${times}" = "${max_time}" ]
	then


		awk -F'\t' '{ if($1 > 180) {long=$1-360} else {long=$1};  printf("%s\t%s\t%.0f\n"), long, $2, $3}' temp/${times}.xyz > temp/everything.xyz
	else
		awk -F'\t' '{ printf("%.0f\n"), $3}' temp/${times}.xyz > temp_file

		paste temp/everything.xyz temp_file > temp_file2
		mv -f temp_file2 temp/everything.xyz
	fi

done


# put everything in the SELEN input file


cat << END_CAT > awk_test.awk
{
use_line = 0;
for ( x = 3; x <= NF; x++ ) {
    if( \$x !=0 && \$x !=-0) {
      use_line=1;
    }
}

if (use_line) print \$0;
}
END_CAT


awk -F'\t' -f awk_test.awk temp/everything.xyz | sed 's/-0/0/g' >> ${file_out}

