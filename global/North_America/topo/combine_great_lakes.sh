#! /bin/bash

# note that the files created here removed any water that is currently below sea level

# Download the NetCDF files from NOAA: https://www.ngdc.noaa.gov/mgg/greatlakes/
# and put them in their respective folders and run plot.sh to produce the water thickness for each lake

#Lake_Erie
#Lake_Huron
#Lake_Michigan
#Lake_Ontario
#Lake_Superior

density_contrast=1.087 # this assumes that the density of freshwater in the Great Lakes is 1000 kg/m^3 and glacial ice is 920 kg/m^3

map_width=15c


x_min=-92.2
x_max=-76.05
y_min=41
y_max=49.5

grdblend Lake_Erie/equivalent_water.nc Lake_Huron/equivalent_water.nc Lake_Michigan/equivalent_water.nc Lake_Ontario/equivalent_water.nc Lake_Superior/equivalent_water.nc -Ggreat_lakes_water_thickness.nc  -R${x_min}/${x_max}/${y_min}/${y_max} -I0.002 -N0


makecpt -Crainbow -T-200/200  -I  > iceshades.cpt
plot=water_thickness.ps
grdimage great_lakes_water_thickness.nc -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}d/0d -K -P -Ciceshades.cpt -V -nb > ${plot}
#grdimage equivalent_water.nc -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}d/0d -K -P -Cshades.cpt -V -nb > ${plot}
pscoast -Bafg -O -K -R -J -P -Wthin -Dh -A500 -Wthin,black >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx100f50+l"water thickness (m)" -G0/200 -Ciceshades.cpt --FONT_LABEL=14p -V  >> $plot


# convert to North America 5 km grid used in ICESHEET


source projection_info.sh




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



echo -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width}

grdproject great_lakes_water_thickness.nc  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -Gwater_thickness_raw.nc -D${spacing}=   -Fe  -V  

grdmath water_thickness_raw.nc 0 DENAN = water_thickness_raw2.nc
grdmath water_thickness_raw2.nc 0 GT = water_thickness_mask.nc
grdmath water_thickness_raw2.nc ${density_contrast} MUL water_thickness_mask.nc MUL = great_lakes_equivalent_ice_thickness.nc


plot=projected_plot.ps

grdimage equivalent_ice_thickness.nc -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Ciceshades.cpt -V -nb > ${plot}

pscoast -Bafg -O -K -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}



psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx100f50+l"equ. ice thickness (m)" -G0/200 -Ciceshades.cpt --FONT_LABEL=14p -V  >> $plot

