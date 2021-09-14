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


original_topo=RTopo-2.0.1_30sec_surface_elevation.nc
original_ice_base=RTopo-2.0.1_30sec_ice_base_topography.nc
original_bedrock_topo=RTopo-2.0.1_30sec_bedrock_topography.nc

topo=topo.nc
ice_base=ice_base.nc
bedrock_topo=bedrock_topo.nc

topo_proj=topo_proj.nc
ice_base_proj=ice_base_proj.nc
bedrock_topo_proj=bedrock_topo_proj.nc

shelf_mask_proj=shelf_mask_proj.nc


source projection_info.sh


# have a bunch of if statements here because doing ncrename and grdproject takes a long time. if you need to redo something, delete the associated file first.

ice_thickness=ice_thickness.nc

# Rtopo is not formatted correctly as a COARDS compliant netcdf file. This command fixes that. Only need to run this once.

if [ ! -e "${bedrock_topo}" ]
then
ncrename -O -d londim,x -d latdim,y -v lon,x -v lat,y ${original_bedrock_topo} ${bedrock_topo}
rm ${shelf_mask} ${ice_thickness}

fi

if [ ! -e "${ice_base}" ]
then
ncrename -O -d londim,x -d latdim,y -v lon,x -v lat,y ${original_ice_base} ${ice_base}

rm ${shelf_mask}
fi


if [ ! -e "${topo}" ]
then
ncrename -O -d londim,x -d latdim,y -v lon,x -v lat,y ${original_topo} ${topo}
rm ${ice_thickness}
fi

shelf_mask="shelf_mask.nc"

# create a mask to remove shelf areas
if [ ! -e "${shelf_mask}" ]
then
echo "calculating shelf mask"
gmt grdmath ${ice_base} ${bedrock_topo} SUB ABS 3 LT = ${shelf_mask}
fi



# extract ice thickness
if [ ! -e "${ice_thickness}" ]
then
echo "calculating ice_thickness"
gmt grdmath ${topo} ${bedrock_topo} SUB ${shelf_mask} MUL = ${ice_thickness}
rm ${greenland_thickness} ${greenland_outline}
fi

greenland_thickness="greenland_thickness.nc"
greenland_outline="ice_contours_outline.gmt"

if [ ! -e "${greenland_thickness}" ]
then
echo "projecting Greenland ice thickness"
gmt grdproject ${ice_thickness}  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -G${greenland_thickness} -D${resolution}000= -Fe  -V 

fi



if [ ! -e "${greenland_outline}" ]
then
gmt makecpt -Cgray -T0/1/0.1  > ice_mask_shades.cpt

if [ ! -e "thick_temp.nc" ]
then
gmt grdproject ${ice_thickness}  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -Gthick_temp.nc -D400= -Fe  -V 
fi

gmt grdmath thick_temp.nc 3 GT = ice_mask.nc



gmt grdcontour ice_mask.nc -Cice_mask_shades.cpt -Q300 -D -L0.45/0.55 -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -P  > ${greenland_outline}



fi


baffin=baffin_ice_thickness.nc
devon=devon_ice_thickness.nc
ellesmere=ellesmere_ice_thickness.nc

area_grid=ice_thickness_proj.nc

gmt grdmath ${greenland_thickness} ${baffin} ADD ${devon} ADD ${ellesmere} ADD = area_temp.nc

gmt grdmath area_temp.nc 1 GT = area_temp_mask.nc

gmt grdmath area_temp.nc area_temp_mask.nc MUL = ${area_grid}

#gmt grdproject ${ice_thickness}  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -G${area_grid} -D${resolution}000= -Fe  -V  


nccopy -k classic ${area_grid} ice_thickness_proj3.nc

# ice thickness plot

plot=ice_thickness.ps
gmt makecpt -Cwysiwyg -T0/5000/250 > shades_ice.cpt

gmt makecpt -Cgray -T0/4000/1000    > iceshades_coarse.cpt

gmt grdimage ${area_grid} -Y12  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Cshades_ice.cpt -V -nb > ${plot}

gmt pscoast -Bafg -O -K -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -P -Wthin -Dl -A5000 -Wthinnest,grey >> ${plot}

gmt psxy ice_contours_outline.gmt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -P -V -Wthin,white >> ${plot}

#gmt grdcontour ${ice_thickness} -Ciceshades_coarse.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

#gmt psxy margins/${time}.gmt  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -K -O -P -V -Wthickest,white >> ${plot}
#gmt psxy margins/${time}.gmt  -R${west_longitude}/${west_latitude}/${east_longitude}/${east_latitude}r -JA${center_longitude}/${center_latitude}/${map_width} -K -O -P -V -Wthin,blue >> ${plot}

gmt psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx1000f500+l"Ice Thickness (m)" --FONT_LABEL=14p -Cshades_ice.cpt -V  >> $plot
