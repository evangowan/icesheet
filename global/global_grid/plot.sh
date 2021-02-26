#! /bin/bash


if [ ! -d "temp" ]
then
   mkdir temp
fi


if [ ! -d "plots" ]
then
   mkdir plots
fi

if [ ! -d "plots/paleo_topography" ]
then
   mkdir plots/paleo_topography
fi

if [ ! -d "plots/relative_sea_level" ]
then
   mkdir plots/relative_sea_level
fi

if [ ! -d "plots/paleo_topography_PS" ]
then
   mkdir plots/paleo_topography_PS
fi


rm temp/*

resolution=0.25
reconstruction=reconstruction_${resolution}_degree.nc

counter=0
for time_slice in 80000 77500 75000 72500 70000 67500 65000 62500 60000 57500 55000 52500 50000 47500 45000 42500 40000 37500 35000 32500 30000 27500 25000 22500 20000 17500 15000 12500 10000 7500 5000 2500 0
do


J_options="-JR-30/18c"
R_options="-Rg"

plot="temp/plot.ps"


makecpt -Cpolar -T-160/160/10  > temp/water_colour.cpt
makecpt -Cetopo1 -G-11000/0 > temp/sea.cpt
makecpt -Cetopo1 -G0/8500 > temp/land.cpt
makecpt -Cjet -T-4500/4500/250  -I  > temp/iceshades.cpt
makecpt -Cgrey,lightblue -T0/2 > temp/grey.cpt

grdmath ${reconstruction}?paleo_topography[${counter}] 0 LT     = temp/sea_only.nc

grdimage temp/sea_only.nc -X1.5 -Y10 ${R_options} ${J_options} -P -K -Q -nn -Ctemp/grey.cpt > ${plot}

grdcontour ${reconstruction}?sea_level[${counter}]  ${R_options} ${J_options} -P -K -O -Wblack -A+f6p,black+gwhite -Gd5c+r0.5c -Ctemp/water_colour.cpt >> ${plot}

grdmath ${reconstruction}?paleo_topography[${counter}] 0 GE  0 NAN ${reconstruction}?paleo_topography[${counter}] MUL = temp/topo_temp.nc

grdimage temp/topo_temp.nc  ${R_options} ${J_options} -Q -P -O -K -nn -Ctemp/land.cpt >> ${plot}

grdmath ${reconstruction}?ice_thickness[${counter}] 0 GT 0 NAN ${reconstruction}?paleo_topography[${counter}] MUL = temp/ice_topo_temp.nc

grdimage temp/ice_topo_temp.nc ${R_options} ${J_options} -Q -P -O -K  -Ctemp/iceshades.cpt >> ${plot}

pscoast -R -J -Bx60 -By30 -B+t"Paleo-Topography - ${time_slice} yr BP" -P -Wfaint,black -Dc -A500 -O -K --FONT_TITLE=16p >> ${plot}


psxy ../margins/Antarctica/${time_slice}.gmt -R -J -P -Wthin,red -O -K >> ${plot}
psxy ../margins/Patagonia/${time_slice}.gmt -R -J -P -Wthin,red -O -K >> ${plot}
psxy ../margins/North_America/${time_slice}.gmt -R -J -P -Wthin,red -O -K >> ${plot}
psxy ../margins/Eurasia/${time_slice}.gmt -R -J -P -Wthin,red -O -K >> ${plot}
psscale -Y-2 -X4  -Dx5c/1c/5c/0.3ch -P -K -O -Bx1000f500+l"Ice Surface Elevation (m)" -G0/4000   -Ctemp/iceshades.cpt --FONT_LABEL=10p --FONT_ANNOT_PRIMARY=8p --FONT_ANNOT_SECONDARY=8p --MAP_TICK_LENGTH_PRIMARY=2p --MAP_ANNOT_OFFSET_PRIMARY=2p --MAP_LABEL_OFFSET=4p  -V  >> $plot

psxy << END_CAT -Y-1.5 -X-4.5 -Gred  -P -K -O -R0/1/0/1 -JX10/10 -Wthin,red >> ${plot}
0.1 0.31
0.2 0.31
END_CAT

pstext << END_CAT   -JX -R -P  -O -F+f8p+jBL -V >> ${plot}
0.23 0.3 Ice Margin
.1 .25 Contours represent relative sea 
.1 .22 level change from present
END_CAT


cp -f ${plot} plots/paleo_topography_PS/${time_slice}.ps
psconvert  ${plot} -Fplots/paleo_topography/${time_slice} -Tf

plot=temp/sea_level_contours.ps

grdimage temp/sea_only.nc -X1.5 -Y10 ${R_options} ${J_options} -P -K -Q -nn -Ctemp/grey.cpt > ${plot}
pscoast -R -J -Bx60 -By30 -B+t"Relative Sea Level- ${time_slice} yr BP" -P -Wfaint,black -Dc -A500 -O -K --FONT_TITLE=16p >> ${plot}
grdcontour ${reconstruction}?sea_level[${counter}]  ${R_options} ${J_options} -P  -O -Wblack -A+f6p,black+gwhite -Gd5c+r0.5c -Ctemp/water_colour.cpt >> ${plot}


psconvert  ${plot} -Fplots/relative_sea_level/${time_slice} -Tf

counter=$( echo "${counter} +  1" | bc )

done
