#! /bin/bash

# use RTopo (Schaffer et al 2016) doi:10.1594/PANGAEA.856844

# change the path to where you store RTopo (or use another topography grid if you want)
# RTopo is convenient because it already takes off the Greenland and Antarctica ice

# switch this to 'y' the first time you run, because Rtopo has an incorrect header. Note, it also does a filter on the data. The grdfilter step, which #
# runs a 5 km median filter on the topography takes like 2 hours!
# Probably could just run it without doing that, but it is likely good in order to prevent aliasing.
# note, if you don't want to do the filtering step, you have to change the grdproject command to use the variable ${topo} instead of ${filtered_topo}
first_run=n


# switch this to 'n' if you have already generated the reduced 
run_project=n


original_topo=RTopo-2.0.1_30sec_bedrock_topography.nc

topo=bed_topo.nc

margin_file=18k.gmt

# Rtopo is not formatted correctly as a COARDS compliant netcdf file. This command fixes that. Only need to run this once.

if [ "${first_run}" == "y" ]
then
ncrename -O -d londim,x -d latdim,y -v lon,x -v lat,y ${original_topo} ${topo}

fi

# load projection information

source ../projection_info.sh


makecpt -Cglobe -T-10000/10000 > shades.cpt

plot=topo_plot.ps

filtered_topo=filtered_topo.nc
area_grid=north_america_topo.nc

# uncomment first time you run, but run only once because it takes a couple of hours!
if [ "${first_run}" == "y" ]
then
grdfilter ${topo} -G${filtered_topo} -Fm${resolution} -D4  -V
fi

# takes a lot less time
#grdproject ${filtered_topo}  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -G${area_grid} -D${resolution}000= -Fe  -V  

# if the Devon ice cap file exists, include it

if [ -e "devon_ice_thickness.nc" ]
then

grdmath ${area_grid} devon_ice_thickness.nc SUB = ${area_grid}


fi


x_min=$(grdinfo -F ${area_grid} | grep x_min  | awk -F':' '{print int($3)}')
x_max=$(grdinfo -F ${area_grid} | grep x_max  | awk -F':' '{print int($3)}')
y_min=$(grdinfo -F ${area_grid} | grep y_min  | awk -F':' '{print int($3)}')
y_max=$(grdinfo -F ${area_grid} | grep y_max  | awk -F':' '{print int($3)}')

grdimage ${area_grid} -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Cshades.cpt -V -nb > ${plot}




psxy ${margin_file}  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${margin_file}  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -K -O -P -V -Wthin,blue >> ${plot}

pscoast -Bafg -O -K -J -R -P -Wthin -Di -A5000 >> ${plot}

test=1

if [ ${test} -eq 1 ]
then
for files in $(seq 1 8)
do

psxy ~/gia/naice_setup/margins/20/22000/22000-000${files}.txt -J -K -O -R -P -V -Wthick,blue >> ${plot}

done

for files in $(seq 1 6)
do

psxy ~/gia/naice_setup/margins/20/20000/20000-000${files}.txt -J -K -O -R -P -V -Wthin,red >> ${plot}

done

for files in $(seq 1 5)
do

psxy ~/gia/naice_setup/margins/20/140000/140000-000${files}.txt -J -K -O -R -P -V -Wthinner,purple >> ${plot}

done

fi

psscale -X-2 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx4000f1000+l"Elevation (m)" --FONT_LABEL=14p -Cshades.cpt -V  >> $plot

# convert to gmt formatted binary file for use in ICESHEET

bin_file="North_America.bin"

grdconvert ${area_grid} ${bin_file}=bf 

echo ${bin_file} > elev_parameters.txt
echo ${x_min} >> elev_parameters.txt
echo ${x_max} >> elev_parameters.txt
echo ${y_min} >> elev_parameters.txt
echo ${y_max} >> elev_parameters.txt
echo ${resolution}000 >> elev_parameters.txt

