#! /bin/bash

# analyzes the 

region="North_America"

spacing=5000
time=0

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

calc_ice_thickness_file="run/0/ice_thickness.nc"

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

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx200f100+l"Ice Thickness (m)" --FONT_LABEL=14p -Cshades_ice_diff.cpt -V  >> $plot


fi


rm shades_ice_diff.cpt shades_ice_diff.cpt

