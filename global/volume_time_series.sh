#! /bin/bash

mkdir temp/

mkdir temp/volumetemp

cd temp/volumetemp

rm *

resolution=5 # in km

region=North_America
#region=Eurasia
#region=Antarctica
#region=Patagonia

your_name="Evan"
run_number="87" 

folder="../../${region}/plots/${your_name}_${run_number}"





modern_ocean_mask=ocean_mask.nc

cp ${folder}/topo/0.nc .

cp ${folder}/thickness/0.nc 0_thickness.nc

gmt grdmath 0.nc 0 LE = ${modern_ocean_mask}

ocean_ice_equivalent="ocean_ice_thickness.nc"

gmt grdmath 0.nc NEG 1027 MUL 910 DIV ${modern_ocean_mask} MUL = ${ocean_ice_equivalent}

zero_thickness=zero.nc

cp -f ${folder}/thickness/0.nc ${zero_thickness}

for ice_file in $(ls ${folder}/thickness | grep .nc)
do

	time=$(echo ${ice_file} | sed 's/.nc//g')
	echo ${time}

	# find overlap with modern ocean

	cp -f ${folder}/thickness/${ice_file} ice_file.nc

	# The nominal thickness does not take into account ice that is below present day sea level

	gmt grdmath ice_file.nc 0_thickness.nc SUB 1000 DIV = sub_thickness.nc # in km

	gmt grdmath sub_thickness.nc ${resolution} MUL ${resolution} MUL SUM  = nominal_volume_sum.nc

	gmt grdtrack -Gnominal_volume_sum.nc << END  | awk -v time=${time} -v resolution=${resolution} '{print time, $3, $3  / 1e6, $3 * 0.91 / 361 / 1e6 * 1000}' >> nominal_volume.txt
0 0
END

	# this removes the ice that is below present day sea level that will not contribute to sea level
	gmt grdmath ice_file.nc 0 GT ${modern_ocean_mask} MUL = overlap.nc

	gmt grdmath ${ocean_ice_equivalent} overlap.nc MUL = overlap_thickness.nc

	gmt grdmath ice_file.nc ${zero_thickness} SUB overlap_thickness.nc SUB 1000 DIV = relative_thickness.nc

	gmt grdmath relative_thickness.nc ${resolution} MUL ${resolution} MUL SUM  = volume_sum.nc

	gmt grdtrack -Gvolume_sum.nc << END 
0 0
END

	gmt grdtrack -Gvolume_sum.nc << END  | awk -v time=${time} -v resolution=${resolution} '{print time, $3, $3  / 1e6, $3 * 0.91 / 361 / 1e6 * 1000}' >> volume.txt
0 0
END




done

sort -n volume.txt > volume_sorted.txt

mv -f volume_sorted.txt ${folder}/volume_sorted.txt


sort -n nominal_volume.txt > nominal_volume_sorted.txt

mv -f nominal_volume_sorted.txt ${folder}/nominal_volume_sorted.txt


cd ../..
