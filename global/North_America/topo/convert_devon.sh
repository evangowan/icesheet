#! /bin/bash

# download the Devon ice cap thickness data from:

# Dowdeswell, J.A., Benham, T.J., Gorman, M.R., Burgess, D. and Sharp, M.J., 2004. Form and flow of the Devon Island ice cap, Canadian Arctic. 
# Journal of Geophysical Research: Earth Surface, 109(F2).

folder="Dowdeswell_et_al_2004_-_data/" # folder containing the Devon ice thickness data
gmt mapproject ${folder}2003JF000095-table3-ice.txt  -hi1 -Ju+17/1:1 -C -I -F  > ice_thickness_ll.txt




# convert to North America 5 km grid used in ICESHEET


source projection_info.sh


gmt mapproject     ice_thickness_ll.txt ${R_options} ${J_options} -F  > ice_thickness_fine.txt

gmt mapproject << END    ${R_options} ${J_options} -F  > corners.txt
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


#gmt grd2xyz -V great_lakes_water_thickness.nc | mapproject  ${R_options} ${J_options} -F > dump.txt

gmt makecpt -Crainbow -T-1000/1000  -I  > iceshades.cpt

gmt blockmedian ice_thickness_fine.txt -R${x_min}/${x_max}/${y_min}/${y_max} -I${spacing}=  -V  -C  > thickness.txt

gmt xyz2grd thickness.txt -R${x_min}/${x_max}/${y_min}/${y_max} -I${spacing}= -Gice_thickness_temp.nc -di0

gmt grdmath ice_thickness_temp.nc 0 DENAN = devon_ice_thickness.nc

#gmt surface thickness.txt -Gice_thickness.nc -I${spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -T0.25 -V 

#echo ${R_options} ${J_options}


plot=projected_plot.ps

gmt grdimage ice_thickness.nc -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Ciceshades.cpt -V -nb > ${plot}

gmt pscoast -Bafg -O -K ${R_options} ${J_options} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}

#gmt grdcontour ice_thickness_coarse.nc -Ciceshades_coarse.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

#gmt psxy margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
#gmt psxy margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}

gmt psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx200f50+l"equ. ice thickness (m)" -G0/1000 -Ciceshades.cpt --FONT_LABEL=14p -V  >> $plot

