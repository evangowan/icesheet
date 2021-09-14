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


original_topo=/scratch/users/egowan-local/topo/RTopo/RTopo-2.0.1_30sec_bedrock_topography.nc

topo=bed_topo.nc

margin_file=mis2_max_extent.gmt

# Rtopo is not formatted correctly as a COARDS compliant netcdf file. This command fixes that. Only need to run this once.

if [ "${first_run}" == "y" ]
then
ncrename -O -d londim,x -d latdim,y -v lon,x -v lat,y ${original_topo} ${topo}

fi

# reading from projection_info.sh now

# For Lambert azimuthal projection

#center_longitude=-94
#center_latitude=60
#resolution=5 # grid resolution, in km!

# corner points of the grid (if we don't use this, gmt assumes a global grid, which will be huge!
# west corresponds to the bottom left corner, east corresponds to the top right corner
# probably easiest to pick off the cordinates off Google Earth, in a really zoomed out view
#west_latitude=25
#west_longitude=-135
#east_latitude=58
#east_longitude=3

#map_width=15c

source ../projection_info.sh




makecpt -Cglobe -T-10000/10000 > shades.cpt

plot=topo_plot.ps

region=Patagonia
cut_topo="cut_topo.nc"
filtered_topo=filtered_topo.nc
area_grid=${region}.nc

# uncomment first time you run, but run only once because it takes a couple of hours!

# well, doing the cut dramatically reduces the time
if [ "${first_run}" == "y" ]
then
gmt grdcut ${topo} -R${extreme_west}/${extreme_east}/${extreme_south}/${extreme_north} -G${cut_topo}

gmt grdfilter ${cut_topo} -G${filtered_topo} -Fm${resolution} -D4  -V
fi

# takes a lot less time
gmt grdproject ${filtered_topo}  ${R_options} ${J_options} -G${area_grid} -D${resolution}000= -Fe  -V  




gmt grdproject ${area_grid}  ${R_options} ${J_options} -G${region}_topo_geo.nc  -I -Fe  -V  


x_min=$(grdinfo -F ${area_grid} | grep x_min  | awk -F':' '{print int($3)}')
x_max=$(grdinfo -F ${area_grid} | grep x_max  | awk -F':' '{print int($3)}')
y_min=$(grdinfo -F ${area_grid} | grep y_min  | awk -F':' '{print int($3)}')
y_max=$(grdinfo -F ${area_grid} | grep y_max  | awk -F':' '{print int($3)}')

gmt grdimage ${area_grid} ${shift_up}  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Cshades.cpt -V -nb > ${plot}




gmt psxy ${margin_file}  ${R_options} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
gmt psxy ${margin_file}  ${R_options} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}

gmt pscoast -Bafg -O -K -J -R -P -Wthin -Di -A5000 >> ${plot}

gmt psscale -X-5 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx4000f1000+l"Elevation (m)" --FONT_LABEL=14p -Cshades.cpt -V  >> $plot

# convert to gmt formatted binary file for use in ICESHEET

bin_file="${region}.bin"

gmt grdconvert ${area_grid} ${bin_file}=bf 

echo ${bin_file} > elev_parameters.txt
echo ${x_min} >> elev_parameters.txt
echo ${x_max} >> elev_parameters.txt
echo ${y_min} >> elev_parameters.txt
echo ${y_max} >> elev_parameters.txt
echo ${resolution}000 >> elev_parameters.txt

# create NetCDF file with equivalent water load

gmt makecpt -Crainbow -T0/4000 -I > shades.cpt

gmt grdmath ${area_grid} 0 LT = ocean_mask.nc

# 1025 / 917 = 1.118 (ratio of density of water to density of ice, used in ICESHEET)
gmt grdmath ${area_grid}  ocean_mask.nc MUL -1.118 MUL = ocean_equivalent_ice.nc

plot=ocean_equivalent.ps

gmt grdimage ocean_equivalent_ice.nc ${shift_up}  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Cshades.cpt -V -nb > ${plot}

#gmt psxy ${margin_file}  ${R_options} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}

gmt pscoast -Bafg -O -K ${R_options} ${J_options} -P -Wthin -Di -A5000 >> ${plot}

gmt psscale -X-5 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx1000f500+l"equivalent ice thickness (m)" -G0/4000 -Cshades.cpt --FONT_LABEL=14p -V  >> $plot
