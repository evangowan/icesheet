#! /bin/bash

# download the Devon ice cap thickness data from:

# Dowdeswell, J.A., Benham, T.J., Gorman, M.R., Burgess, D. and Sharp, M.J., 2004. Form and flow of the Devon Island ice cap, Canadian Arctic. 
# Journal of Geophysical Research: Earth Surface, 109(F2).

folder="Dowdeswell_et_al_2004_-_data/" # folder containing the Devon ice thickness data
mapproject ${folder}2003JF000095-table3-ice.txt  -hi1 -Ju+17/1:1 -C -I -F  > ice_thickness_ll.txt




# convert to North America 5 km grid used in ICESHEET


source projection_info.sh


mapproject     ice_thickness_ll.txt -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F  > ice_thickness_fine.txt

mapproject << END    -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F  > corners.txt
${west_longitude} ${west_latitude}
${east_longitude} ${east_latitude}
END

spacing=5000
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


#grd2xyz -V great_lakes_water_thickness.nc | mapproject  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -F > dump.txt

makecpt -Crainbow -T-1000/1000  -I  > iceshades.cpt

blockmedian ice_thickness_fine.txt -R${x_min}/${x_max}/${y_min}/${y_max} -I${spacing}=  -V  -C  > thickness.txt

xyz2grd thickness.txt -R${x_min}/${x_max}/${y_min}/${y_max} -I${spacing}= -Gice_thickness_temp.nc -di0

grdmath ice_thickness_temp.nc 0 DENAN = devon_ice_thickness.nc

#surface thickness.txt -Gice_thickness.nc -I${spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -T0.25 -V 

#echo -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width}


plot=projected_plot.ps

grdimage ice_thickness.nc -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Ciceshades.cpt -V -nb > ${plot}

pscoast -Bafg -O -K -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}

#grdcontour ice_thickness_coarse.nc -Ciceshades_coarse.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

#psxy margins/${time}.gmt  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -K -O -P -V -Wthickest,white >> ${plot}
#psxy margins/${time}.gmt  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -K -O -P -V -Wthin,blue >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx200f50+l"equ. ice thickness (m)" -G0/1000 -Ciceshades.cpt --FONT_LABEL=14p -V  >> $plot

