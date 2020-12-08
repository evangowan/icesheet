#! /bin/bash

lake_grid=huron_lld.grd
lake_height=177 # in m from the Army Corps of Engineers, long term average

x_min=-84.5
x_max=-79.68
y_min=43
y_max=46.5

map_width=15c

makecpt -Cglobe -T-10000/10000 > shades.cpt
plot=elevation.ps
grdimage ${lake_grid} -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}d/0d -K -P -Cshades.cpt -V -nb > ${plot}
pscoast -Bafg -O  -R -J -P -Wthin -Dh -A500 -Wthin,black >> ${plot}

# manipulate

#grdmath ${lake_grid} -${lake_height} GT ${lake_grid} ADD = deep_mask.nc
grdmath ${lake_grid} -${lake_height} LT ${lake_height} MUL  = deep_mask.nc

makecpt -Cjet -T0/200/25  -I  > maskshades.cpt
#makecpt -Cjet -T0/1/0.2  -I  > maskshades.cpt
plot=deep_mask.ps
grdimage deep_mask.nc -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}d/0d -K -P -Cmaskshades.cpt -V -nb > ${plot}


grdmath ${lake_grid} -${lake_height} GE = high_mask.nc

makecpt -Cjet -T0/1/0.2  -I  > maskshades.cpt
plot=high_mask.ps
grdimage high_mask.nc -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}d/0d -K -P -Cmaskshades.cpt -V -nb > ${plot}


grdlandmask -Gland_mask.nc -R${lake_grid} -Df  -N1/0

plot=land_mask.ps
grdimage land_mask.nc -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}d/0d -K -P -Cmaskshades.cpt -V -nb > ${plot}

grdmath land_mask.nc high_mask.nc MUL  ${lake_grid} MUL -1 MUL deep_mask.nc ADD = equivalent_water_temp.nc

grdmath equivalent_water_temp.nc 0 GT equivalent_water_temp.nc MUL = equivalent_water.nc


makecpt -Cjet -T-400/400/25  -I  > iceshades.cpt
plot=water_thickness.ps
grdimage equivalent_water.nc -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}d/0d -K -P -Ciceshades.cpt -V -nb > ${plot}
#grdimage equivalent_water.nc -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}d/0d -K -P -Cshades.cpt -V -nb > ${plot}
pscoast -Bafg -O -K -R -J -P -Wthin -Dh -A500 -Wthin,black >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx100f50+l"water thickness (m)" -G0/400 -Ciceshades.cpt --FONT_LABEL=14p -V  >> $plot
