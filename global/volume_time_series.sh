#! /bin/bash

mkdir temp/

mkdir temp/volumetemp

cd temp/volumetemp

rm *

resolution=5 # in km

#region=North_America
#region=Eurasia
#region=Antarctica
region=Patagonia

your_name="Evan"
run_number="65" 

folder="../../${region}/plots/${your_name}_${run_number}"





modern_ocean_mask=ocean_mask.nc

cp ${folder}/topo/0.nc .

grdmath 0.nc 0 LE = ${modern_ocean_mask}

ocean_ice_equivalent="ocean_ice_thickness.nc"

grdmath 0.nc NEG 1027 MUL 910 DIV ${modern_ocean_mask} MUL = ${ocean_ice_equivalent}

zero_thickness=zero.nc

cp -f ${folder}/thickness/0.nc ${zero_thickness}

for ice_file in $(ls ${folder}/thickness | grep .nc)
do

	time=$(echo ${ice_file} | sed 's/.nc//g')
	echo ${time}

	# find overlap with modern ocean

	cp -f ${folder}/thickness/${ice_file} ice_file.nc

	grdmath ice_file.nc 0 GT ${modern_ocean_mask} MUL = overlap.nc

	grdmath ${ocean_ice_equivalent} overlap.nc MUL = overlap_thickness.nc

	grdmath ice_file.nc ${zero_thickness} SUB overlap_thickness.nc SUB 1000 DIV = relative_thickness.nc

	grdmath relative_thickness.nc ${resolution} MUL ${resolution} MUL SUM  = volume_sum.nc

	grdtrack -Gvolume_sum.nc << END 
0 0
END

	grdtrack -Gvolume_sum.nc << END  | awk -v time=${time} -v resolution=${resolution} '{print time, $3, $3  / 1e6, $3 * 0.91 / 361 / 1e6 * 1000}' >> volume.txt
0 0
END




done

sort -n volume.txt > volume_sorted.txt

mv -f volume_sorted.txt ${folder}/volume_sorted.txt


cd ../..
