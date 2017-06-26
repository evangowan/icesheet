#! /bin/bash

# this is used purely for the run directory



# Ice margin file for the desired time
time=$(awk '{if (NR == 1) print $0}' ../run_parameters)
adjust_file=$(awk '{if (NR == 17) print $0}' ../run_parameters)

root_directory=$(awk '{if (NR==1) print $0}' root_directory)

margin_file=../margins/${time}.gmt


bin_path="${root_directory}/.." # leave blank if create_ss_grid is in ${PATH}

# The GMT formatted text file needs to be created before running this script.
# Steps:
# 1) Open shear_stress_domains_reproj.shp in QGIS
# 2) Right click on "shear_stress_domains_reproj " in the Layers dialog and click on "save as"
# 3) In format, select "Generic Mapping Tools (GMT)"; in save as, find the path to the current directory and use the file name "shear_stress_domains.gmt"; 
#    in Symbology export, select "feature symbology"; then hit save 

domain_gmt_file=shear_stress_domains.gmt

# load projection information

source ../projection_info.sh



mapproject << END    -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F  > corners.txt
${west_longitude} ${west_latitude}
${east_longitude} ${east_latitude}
END

spacing=${resolution}000

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





bin_file="shear_stress.bin"

makecpt -Cwysiwyg -T0/200000/10000 -I > shades_shearstress.cpt

cat << END_CAT > ss_parameters.txt
shear_stress.bin
${x_min}
${x_max}
${y_min}
${y_max}
${spacing}
END_CAT

mapproject << END    -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F >> ss_parameters.txt
${center_longitude} ${center_latitude}
END



# convert the GMT file into a binary grid

echo ./${bin_path}/create_ss_grid


if [ "${adjust_file}" = "" ]
then
	./${bin_path}/create_ss_grid ${domain_gmt_file} domains_max.txt
else
	if [ -f "domains_min.txt" ]
	then
		./${bin_path}/create_ss_grid ${domain_gmt_file} domains_max.txt ${adjust_file} ${time} domains_min.txt
	else
		./${bin_path}/create_ss_grid ${domain_gmt_file} domains_max.txt ${adjust_file} ${time}
	fi
fi

nc_file=shear_stress.nc

xyz2grd shear_stress_grid.txt -I${spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -G${nc_file}

# plot the file

plot="shear_stress.ps"

grdimage ${nc_file} -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Cshades_shearstress.cpt -V -nb > ${plot}


mapproject << END    -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F -C > corners.txt
${west_longitude} ${west_latitude}
${east_longitude} ${east_latitude}
END

r1=$(awk '{if (NR==1) print $1}' corners.txt)
r2=$(awk '{if (NR==2) print $1}' corners.txt)
r3=$(awk '{if (NR==1) print $2}' corners.txt)
r4=$(awk '{if (NR==2) print $2}' corners.txt)

psxy ${domain_gmt_file}  -R${r1}/${r2}/${r3}/${r4} -JX -K -O -P -V -Wthin >> ${plot}

pscoast -Bafg -O -K -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -P -Wthin -Di -A5000 -Wthinnest,grey >> ${plot}

psxy ${margin_file}  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${margin_file}  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -K -O -P -V -Wthin,blue >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx100000f20000+l"Shear Stress (Pa)" --FONT_LABEL=14p -Cshades_shearstress.cpt -V  >> $plot



# convert the grid to a format used by ICESHEET (GMT binary)

grdconvert ${nc_file} ${bin_file}=bf 
