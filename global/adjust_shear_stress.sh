#! /bin/bash

# This script is used to determine how different the ice sheet reconstruction is from modern day ice thickness

region="North_America"

spacing=5000
time=0


domains_min=${region}/shear_stress/domains_min.txt
domains_max=${region}/shear_stress/domains_max.txt

source ${region}/projection_info.sh

mapproject << END    ${R_options} ${J_options} -F  > corners.txt
${west_longitude} ${west_latitude}
${east_longitude} ${east_latitude}
END





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


# the shear stress boundaries file was created using qgis, so the zero point is in the middle of the grid rather than the bottom left

mapproject << END    ${R_options} ${J_options} -F  > projection_center.txt
${center_longitude} ${center_latitude}
END

center_x=$( awk '{print $1}' projection_center.txt)
center_y=$( awk '{print $2}' projection_center.txt)



ss_domain_boundaries="shear_stress_boundaries.gmt"

awk -v xshift=${center_x} -v yshift=${center_y} '{if ($1 != "#" && $1 != ">" ) {print $1+xshift, $2+yshift} else {print $0} }' ${region}/shear_stress/shear_stress_domains.gmt > ${ss_domain_boundaries}

ice_thickness_file=${region}/topo/ice_thickness_proj.nc


# assuming that you ran time zero

calc_ice_thickness_file="run/${time}/ice_thickness.nc"

plot_dir="${region}/plots/thickness_compare"

mkdir ${plot_dir}



plot=${plot_dir}/ice_thickness_diff.ps

mask_file="ice_diff_mask.nc"
grdmath ${ice_thickness_file} 2 GT = ${mask_file}

diff_file="ice_diff.nc"
grdmath ${ice_thickness_file} ${calc_ice_thickness_file} SUB ${mask_file} MUL = ${diff_file}



makecpt -Cwysiwyg -T-550/550/100 > shades_ice_diff.cpt

makecpt -Cgray -T-1000/1000/500    > iceshades_coarse_diff.cpt


grdimage ${diff_file} ${shift_up}  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Cshades_ice_diff.cpt -V -nb > ${plot}

pscoast -Bafg -O -K ${R_options} ${J_options} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}

#grdcontour ${diff_file} -Ciceshades_coarse.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

psxy ${region}/margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${region}/margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}


psxy ${ss_domain_boundaries} -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -P -V -Wthin,black >> ${plot}



psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx200f100+l"Ice Thickness (m)" --FONT_LABEL=14p -Cshades_ice_diff.cpt -V  >> $plot

# let's make a zoomed in map


if [ ${region} = "North_America" ]
then

west_longitude2=-100
west_latitude2=65
east_longitude2=5
east_latitude2=65

R_options2="-R${west_longitude2}/${west_latitude2}/${east_longitude2}/${east_latitude2}r"

mapproject << END    ${R_options} ${J_options} -F  > corners.txt
${west_longitude2} ${west_latitude2}
${east_longitude2} ${east_latitude2}
END

r1=$(awk '{if (NR==1) print $1}' corners.txt)
r2=$(awk '{if (NR==2) print $1}' corners.txt)
r3=$(awk '{if (NR==1) print $2}' corners.txt)
r4=$(awk '{if (NR==2) print $2}' corners.txt)

# round the numbers, should only need to do this for the top left corner, really

x_min2=${r1}
y_min2=${r3}
x_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r2} / ${spacing}" | bc ) )
x_max2=$(echo "${x_max_temp} * ${spacing}" | bc)
y_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r4} / ${spacing}" | bc ) )
y_max2=$(echo "${y_max_temp} * ${spacing}" | bc)


plot=${plot_dir}/ice_thickness_diff2.ps


grdimage ${diff_file} ${shift_up}  -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 -K -P -Cshades_ice_diff.cpt -V -nb > ${plot}

pscoast -Bafg -O -K ${R_options2} ${J_options} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}

#grdcontour ${diff_file} -Ciceshades_coarse.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

psxy ${region}/margins/${time}.gmt  ${R_options2} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${region}/margins/${time}.gmt  ${R_options2} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}


psxy ${ss_domain_boundaries} -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 -K -O -P -V -Wthin,black >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx200f100+l"Ice Thickness difference (m)" --FONT_LABEL=14p -Cshades_ice_diff.cpt -V  >> $plot


fi




# extract the shear stress domain numbers for each domain polygon

grep "# @D" ${ss_domain_boundaries} | awk -F'|' '{print $2}' > shear_stress_polygon_ids.txt

grd2xyz ${diff_file} > diff_file.xyz

cat << END_CAT > diff_map_params.txt
${x_min}
${x_max}
${y_min}
${y_max}
${spacing}
${ss_domain_boundaries}
shear_stress_polygon_ids.txt
diff_file.xyz
END_CAT


# this program creates gmt files and statistic files
./../diff_map





mean_file=mean_diff.gmt
median_file=median_diff.gmt


makecpt -Cwysiwyg -T-275/275/50 > shades_ice_diff.cpt

# mean plot
plot=${plot_dir}/ice_thickness_diff_regions_mean.ps
psxy ${mean_file}  ${shift_up} -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -L -Cshades_ice_diff.cpt -V -K -Wthin,black -P > ${plot}


pscoast -Bafg -O -K ${R_options} ${J_options} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}

#grdcontour ${diff_file} -Ciceshades_coarse.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

psxy ${region}/margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${region}/margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}


psxy ${ss_domain_boundaries} -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -P -V -Wthin,black >> ${plot}



psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx100f50+l"Ice Thickness difference (m)" --FONT_LABEL=14p -Cshades_ice_diff.cpt -V  >> $plot


# median plot
plot=${plot_dir}/ice_thickness_diff_regions_median.ps
psxy ${median_file}  ${shift_up} -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -L -Cshades_ice_diff.cpt -V -K -Wthin,black -P > ${plot}


pscoast -Bafg -O -K ${R_options} ${J_options} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}

#grdcontour ${diff_file} -Ciceshades_coarse.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

psxy ${region}/margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${region}/margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}


psxy ${ss_domain_boundaries} -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -P -V -Wthin,black >> ${plot}



psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx100f50+l"Ice Thickness difference (m)" --FONT_LABEL=14p -Cshades_ice_diff.cpt -V  >> $plot




if [ ${region} = "North_America" ]
then

west_longitude2=-100
west_latitude2=65
east_longitude2=5
east_latitude2=65

R_options2="-R${west_longitude2}/${west_latitude2}/${east_longitude2}/${east_latitude2}r"

mapproject << END    ${R_options} ${J_options} -F  > corners.txt
${west_longitude2} ${west_latitude2}
${east_longitude2} ${east_latitude2}
END

r1=$(awk '{if (NR==1) print $1}' corners.txt)
r2=$(awk '{if (NR==2) print $1}' corners.txt)
r3=$(awk '{if (NR==1) print $2}' corners.txt)
r4=$(awk '{if (NR==2) print $2}' corners.txt)

# round the numbers, should only need to do this for the top left corner, really

x_min2=${r1}
y_min2=${r3}
x_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r2} / ${spacing}" | bc ) )
x_max2=$(echo "${x_max_temp} * ${spacing}" | bc)
y_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r4} / ${spacing}" | bc ) )
y_max2=$(echo "${y_max_temp} * ${spacing}" | bc)


plot=${plot_dir}/ice_thickness_diff_regions_mean_zoom.ps
psxy ${mean_file}  ${shift_up} -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 -L -Cshades_ice_diff.cpt -V -K -Wthin,black -P > ${plot}

pscoast -Bafg -O -K ${R_options2} ${J_options} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}

#grdcontour ${diff_file} -Ciceshades_coarse.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

psxy ${region}/margins/${time}.gmt  ${R_options2} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${region}/margins/${time}.gmt  ${R_options2} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}


psxy ${ss_domain_boundaries} -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 -K -O -P -V -Wthin,black >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx200f100+l"Ice Thickness difference (m)" --FONT_LABEL=14p -Cshades_ice_diff.cpt -V  >> $plot



plot=${plot_dir}/ice_thickness_diff_regions_median_zoom.ps
psxy ${median_file}  ${shift_up} -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 -L -Cshades_ice_diff.cpt -V -K -Wthin,black -P > ${plot}

pscoast -Bafg -O -K ${R_options2} ${J_options} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}

#grdcontour ${diff_file} -Ciceshades_coarse.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

psxy ${region}/margins/${time}.gmt  ${R_options2} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${region}/margins/${time}.gmt  ${R_options2} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}


psxy ${ss_domain_boundaries} -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 -K -O -P -V -Wthin,black >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx200f100+l"Ice Thickness difference (m)" --FONT_LABEL=14p -Cshades_ice_diff.cpt -V  >> $plot




# just thickness
makecpt -Cwysiwyg -T0/4000/250 > shades_ice.cpt
makecpt -Cgray -T0/4000/1000    > iceshades_coarse.cpt
plot=${plot_dir}/ice_thickness_zoom.ps
grdimage ${calc_ice_thickness_file} ${shift_up}  -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 -K -P -Cshades_ice.cpt -V -nb > ${plot}

pscoast -Bafg -O -K ${R_options2} ${J_options} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}

grdcontour run/${time}/ice_thickness_coarse.nc -Ciceshades_coarse.cpt -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

psxy ${region}/margins/${time}.gmt  ${R_options2} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${region}/margins/${time}.gmt  ${R_options2} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}


#psxy ${ss_domain_boundaries} -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 -K -O -P -V -Wthin,black >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx1000f500+l"Ice Thickness (m)" --FONT_LABEL=14p -Cshades_ice.cpt -V  >> $plot

# just elevation

plot=${plot_dir}/ice_elevation_zoom.ps

makecpt -Cjet -T-4000/4000/250  -I  > iceshades.cpt

makecpt -Cgray -T0/4000/500    > iceshades_coarse.cpt

makecpt -Cglobe -T-10000/10000 > shades.cpt
grdimage run/${time}/${region}.nc ${shift_up}  -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 -K -P -Cshades.cpt -V -nb > ${plot}

pscoast -Bafg -O -K ${R_options2} ${J_options} -P -Wthin -Di -A5000 -Wthin,black >> ${plot}

psclip run/${time}/margins/${time}_proj.gmt -K -O -R${x_min2}/${x_max2}/${y_min2}/${y_max2}  -JX${map_width}/0 >> $plot

grdimage run/${time}/ice_topo.nc -Ciceshades.cpt -J -R -V -P -nb+a+bg+t0.1 -K  -O >> ${plot}

grdcontour run/${time}/ice_topo_coarse.nc -Ciceshades_coarse.cpt -R -J -K -O -W+0.75p -A+f8p,black+gwhite >> ${plot}

psclip -K -O -C  >> $plot



psxy ${region}/margins/${time}.gmt  ${R_options2} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${region}/margins/${time}.gmt  ${R_options2} ${J_options} -K -O -P -V -Wthin,red >> ${plot}


psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx1000f500+l"Ice elevation (m)" -G0/4000 -Ciceshades.cpt --FONT_LABEL=14p -V  >> $plot


# more zoom

contour_file=run/${time}/contours/24.contours

awk '{if($1==">") {print $1, $2 $3; elevation=int($3)} else {if(int($3+0.1) == elevation) {print $1, $2} else{print ">-Z" elevation}}}' ${contour_file} > contour_temp_file.txt

west_longitude3=-52
west_latitude3=68.75
east_longitude3=-42
east_latitude3=68

R_options3="-R${west_longitude3}/${west_latitude3}/${east_longitude3}/${east_latitude3}r"

mapproject << END    ${R_options} ${J_options} -F  > corners.txt
${west_longitude3} ${west_latitude3}
${east_longitude3} ${east_latitude3}
END

r1=$(awk '{if (NR==1) print $1}' corners.txt)
r2=$(awk '{if (NR==2) print $1}' corners.txt)
r3=$(awk '{if (NR==1) print $2}' corners.txt)
r4=$(awk '{if (NR==2) print $2}' corners.txt)

# round the numbers, should only need to do this for the top left corner, really

x_min3=${r1}
y_min3=${r3}
x_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r2} / ${spacing}" | bc ) )
x_max3=$(echo "${x_max_temp} * ${spacing}" | bc)
y_max_temp=$(printf '%.0f\n' $(echo "scale=2; ${r4} / ${spacing}" | bc ) )
y_max3=$(echo "${y_max_temp} * ${spacing}" | bc)


thickness_dump_file=run/${time}/ice_thickness.txt

coarse_spacing=10000
grdmask run/${time}/margins/${time}_proj.gmt -I${coarse_spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -Gmask_less_coarse.nc

blockmedian ${thickness_dump_file} -R${x_min}/${x_max}/${y_min}/${y_max} -I${spacing}=   -C  > reconstruction_thickness.txt

surface reconstruction_thickness.txt -Gice_thickness_raw.nc -I${coarse_spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -T0.25 -V 

grdmath ice_thickness_raw.nc mask_less_coarse.nc MUL = ice_thickness_less_coarse.nc


makecpt -Cglobe -T-10000/10000 > shades.cpt
plot=${plot_dir}/ice_elevation_zoom_lines.ps
grdimage run/${time}/${region}.nc ${shift_up}  -R${x_min3}/${x_max3}/${y_min3}/${y_max3}  -JX${map_width}/0 -K -P -Cshades.cpt -V -nb > ${plot}
pscoast  -Ba -Bwens -O -K ${R_options3} ${J_options} -P  -Df -A5000 -Wthin,darkgrey >> ${plot}
makecpt -Cjet -T-4000/4000/100  -I  > iceshades_fine.cpt
psclip run/${time}/margins/${time}_proj.gmt -K -O -R${x_min3}/${x_max3}/${y_min3}/${y_max3}  -JX${map_width}/0 >> $plot

grdcontour ice_thickness_less_coarse.nc -Ciceshades_fine.cpt -R -J -K -O -W+0.75p -A+f8p,black+gwhite >> ${plot}
#psxy contour_temp_file.txt -Ciceshades_fine.cpt -R${x_min3}/${x_max3}/${y_min3}/${y_max3}  -JX${map_width}/0 -K -O -W0.75p  >> ${plot}

psclip -K -O -C  >> $plot

psxy ${region}/margins/${time}.gmt  ${R_options3} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${region}/margins/${time}.gmt  ${R_options3} ${J_options}  -O -P -V -Wthin,red >> ${plot}


plot=${plot_dir}/ice_shear_stress_zoom_lines.ps

makecpt -Cwysiwyg -T0/200000/10000 -I > shades_shearstress.cpt



grdimage run/${time}/shear_stress/shear_stress.nc ${shift_up}  -R${x_min3}/${x_max3}/${y_min3}/${y_max3}  -JX${map_width}/0 -K -P -Cshades_shearstress.cpt -V -nb > ${plot}
pscoast -Ba -Bwens -O -K ${R_options3} ${J_options}  -P -Dh -A5000 -Wthin,darkgrey >> ${plot}

psxy run/${time}/shear_stress/shear_stress_domains.gmt  -R${x_min3}/${x_max3}/${y_min3}/${y_max3} -JX${map_width}/0 -K -O -P -V -Wthin >> ${plot}

psclip run/${time}/margins/${time}_proj.gmt -K -O -R${x_min3}/${x_max3}/${y_min3}/${y_max3}  -JX${map_width}/0 >> $plot

grdcontour ice_thickness_less_coarse.nc -Ciceshades_fine.cpt -R -J -K -O -W+0.75p -A+f8p,black+gwhite >> ${plot}
#psxy contour_temp_file.txt -Ciceshades_fine.cpt -R${x_min3}/${x_max3}/${y_min3}/${y_max3}  -JX${map_width}/0 -K -O -W0.75p  >> ${plot}

psclip -K -O -C  >> $plot
psxy ${region}/margins/${time}.gmt  ${R_options3} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy ${region}/margins/${time}.gmt  ${R_options3} ${J_options} -K -O -P -V -Wthin,red >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx100000f20000+l"Shear Stress (Pa)" --FONT_LABEL=14p -Cshades_shearstress.cpt -V  >> $plot

fi


# make copy of the domains files for testing


cp -f ${domains_max} domains_max_temp.txt
cp -f ${domains_min} domains_min_temp.txt

diff_file=mean_diff_id.txt

# for now assuming that the adjust file doesn't need to be changed
#./../adjust_ss  ${diff_file} ${domains_min} ${domains_max}

#./../adjust_ss  ${diff_file} domains_min_temp.txt domains_max_temp.txt

rm shades_ice_diff.cpt shades_ice_diff.cpt
