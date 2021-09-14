#! /bin/bash

#ICEBRIDGE MCoRDS derived ice thickness data.

#Leuschen, C., P. Gogineni, F. Rodriguez-Morales, J. Paden, and C. Allen. 2010, updated 2017. IceBridge MCoRDS L2 Ice Thickness, Version 1. Boulder, Colorado USA. NASA National Snow and Ice Data Center Distributed Active Archive Center. doi: http://dx.doi.org/10.5067/GDQ0CUCVTE2Q


base_name=ellesmere
ice_margin_file=${base_name}_margins.gmt

cat << END_CAT > file_lists
IRMCR2_20110316_14.csv
IRMCR2_20110505_01.csv
IRMCR2_20110507_02.csv
IRMCR2_20110510_02.csv
IRMCR2_20110510_03.csv
IRMCR2_20110510_04.csv
IRMCR2_20120517_02.csv
IRMCR2_20130425_01.csv
IRMCR2_20140325_05.csv
IRMCR2_20140325_06.csv
IRMCR2_20140325_07.csv
IRMCR2_20140331_02.csv
IRMCR2_20140401_01.csv
IRMCR2_20140401_03.csv
IRMCR2_20140401_04.csv
IRMCR2_20140506_01.csv
END_CAT

ice_thickness_file=ice_thickness.txt

rm ${ice_thickness_file}
for file in $(cat file_lists)
do

echo ">" >> ${ice_thickness_file}
awk -F, 'START {last=""}; {if (NR > 1 && $4 > 0) {print $2, $1, $4; last=$2} else if (last != ">") {print ">"; last = ">"} }' $file > thickness_temp



gmt gmtselect thickness_temp -F${ice_margin_file} >> ${ice_thickness_file}

echo something

done



source projection_info.sh

map_width=15c


x_min=-95
x_max=-60
y_min=67
y_max=85

west_latitude2=76
west_longitude2=-96.5
east_latitude2=83
east_longitude2=-54

map_width=10c


spacing=5000

gmt mapproject << END   ${R_options} ${J_options} -F  > corners.txt
${west_longitude} ${west_latitude}
${east_longitude} ${east_latitude}
END

r1=$(awk '{if (NR==1) print $1}' corners.txt)
r2=$(awk '{if (NR==2) print $1}' corners.txt)
r3=$(awk '{if (NR==1) print $2}' corners.txt)
r4=$(awk '{if (NR==2) print $2}' corners.txt)
# round the numbers, should only need to do this for the top left corner, really

x_min_temp=$(printf '%.0f\n' $(echo "scale=2; ${r1} / ${spacing}" | bc ) )
x_min=$(echo "${x_min_temp} * ${spacing}" | bc)
y_min_temp=$(printf '%.0f\n' $(echo "scale=2; ${r3} / ${spacing}" | bc ) )
y_min=$(echo "${y_min_temp} * ${spacing}" | bc)
x_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r2} / ${spacing}" | bc ) )
x_max=$(echo "${x_max_temp} * ${spacing}" | bc)
y_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r4} / ${spacing}" | bc ) )
y_max=$(echo "${y_max_temp} * ${spacing}" | bc)

echo -R${x_min}/${x_max}/${y_min}/${y_max}



awk '{if($1 != "#" && $1 !=">") print $1, $2, 0}' ${ice_margin_file} > thickness_dump.txt
awk '{if($1 != "#" && $1 !=">") print $1, $2, $3}' ${ice_thickness_file} >> thickness_dump.txt


gmt mapproject thickness_dump.txt    ${R_options} ${J_options} -F  > thickness_dump_proj.txt

gmt blockmedian thickness_dump_proj.txt -R${x_min}/${x_max}/${y_min}/${y_max} -I${spacing}  -V  -C  > median_dump.txt

gmt surface median_dump.txt -Gice_thickness_raw.nc -I${spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -T0.75 -V 

gmt mapproject ${ice_margin_file} ${R_options} ${J_options} -F  > margin_proj.txt

gmt grdmask margin_proj.txt -R${x_min}/${x_max}/${y_min}/${y_max} -I${spacing}= -Gice_mask.nc

gmt grdmask margin_proj.txt -R${x_min}/${x_max}/${y_min}/${y_max} -I${spacing}= -NNaN/1/1 -Gice_mask_plot.nc

gmt grdmath ice_thickness_raw.nc ice_mask.nc MUL = ${base_name}_ice_thickness.nc

gmt grdmath ice_thickness_raw.nc ice_mask_plot.nc MUL = ${base_name}_ice_thickness_plot.nc

max_thickness_scale=1000

gmt makecpt -Crainbow -T-${max_thickness_scale}/${max_thickness_scale}  -I  > iceshades.cpt


gmt mapproject << END   ${R_options} ${J_options} -F  > corners.txt
${west_longitude2} ${west_latitude2}
${east_longitude2} ${east_latitude2}
END

r1=$(awk '{if (NR==1) print $1}' corners.txt)
r2=$(awk '{if (NR==2) print $1}' corners.txt)
r3=$(awk '{if (NR==1) print $2}' corners.txt)
r4=$(awk '{if (NR==2) print $2}' corners.txt)
# round the numbers, should only need to do this for the top left corner, really

x_min_temp=$(printf '%.0f\n' $(echo "scale=2; ${r1} / ${spacing}" | bc ) )
x_min=$(echo "${x_min_temp} * ${spacing}" | bc)
y_min_temp=$(printf '%.0f\n' $(echo "scale=2; ${r3} / ${spacing}" | bc ) )
y_min=$(echo "${y_min_temp} * ${spacing}" | bc)
x_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r2} / ${spacing}" | bc ) )
x_max=$(echo "${x_max_temp} * ${spacing}" | bc)
y_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r4} / ${spacing}" | bc ) )
y_max=$(echo "${y_max_temp} * ${spacing}" | bc)

plot=thickness_${base_name}.ps
#gmt psxy ${ice_thickness_file} -X2 -Y10  ${R_options} ${J_options} -K -P -Sc0.2 -Ciceshades.cpt -V  > ${plot}

gmt grdimage ${base_name}_ice_thickness_plot.nc -Q -Y12 -X4 -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Ciceshades.cpt -V -nb > ${plot}

gmt psxy ${ice_thickness_file}   -R${west_longitude2}/${west_latitude2}/${east_longitude2}/${east_latitude2}r ${J_options} -K -P -O -V -Wthick,grey >> ${plot}
gmt psxy ${ice_thickness_file}   -R${west_longitude2}/${west_latitude2}/${east_longitude2}/${east_latitude2}r ${J_options} -K -P -O -V -Wthinnest,black >> ${plot}
gmt psxy ${ice_margin_file} -R${west_longitude2}/${west_latitude2}/${east_longitude2}/${east_latitude2}r ${J_options} -K -O -Wthickest,red >> ${plot}
gmt pscoast -Bafg -O -K -R -J -P -Wthin -Dh -A500 -Wthin,black >> ${plot}

gmt psscale -X-4 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx200f50+l"ice thickness (m)" -G0/${max_thickness_scale} -Ciceshades.cpt --FONT_LABEL=14p -V  >> $plot
