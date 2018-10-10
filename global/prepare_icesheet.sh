#! /bin/bash

# load GMT module

module load GMT

# if ICESHEET is not in a place visible by ${PATH}, add it here
PATH=${PATH}:/work/ollie/egowan/icesheet/icesheet


time=$(awk '{if (NR == 1) print $0}' run_parameters)
region=$(awk '{if (NR == 2) print $0}' run_parameters)
run_number=$(awk '{if (NR == 3) print $0}' run_parameters)
run_description=$(awk '{if (NR == 4) print $0}' run_parameters)
earth_model=$(awk '{if (NR == 5) print $0}' run_parameters)
North_America_run_number=$(awk '{if (NR == 6) print $0}' run_parameters)
Eurasia_run_number=$(awk '{if (NR == 7) print $0}' run_parameters)
Antarctica_run_number=$(awk '{if (NR == 8) print $0}' run_parameters)
icesheet_spacing=$(awk '{if (NR == 9) print $0}' run_parameters)
icesheet_interval=$(awk '{if (NR == 10) print $0}' run_parameters)
latitude_spacing=$(awk '{if (NR == 11) print $0}' run_parameters)
longitude_spacing=$(awk '{if (NR == 12) print $0}' run_parameters)

your_name=$(awk '{if (NR == 15) print $0}' run_parameters)
gia_deformation=$(awk '{if (NR == 16) print $0}' run_parameters)
adjust_file=$(awk '{if (NR == 17) print $0}' run_parameters)
folder_on=$(awk '{if (NR == 18) print $0}' run_parameters)

echo ${region}
echo ${run_number}
echo ${run_description}
echo ${earth_model}
echo ${North_America_run_number}
echo ${Eurasia_run_number}
echo ${Antarctica_run_number}
echo ${icesheet_spacing}
echo ${icesheet_interval}
echo ${latitude_spacing}
echo ${longitude_spacing}
echo ${gia_deformation}

root_directory="../.."

spacing=${icesheet_spacing}000

mkdir plots

nc_grid=${root_directory}/${region}/topo/${region}.nc

# load projection information
cp ${root_directory}/${region}/projection_info.sh .

source projection_info.sh

if [ "${special_projection}" = "y" ]
then

mapproject << END    ${R_options} ${J_options_project} -C -F  > corners.txt
${west_longitude} ${west_latitude}
${east_longitude} ${east_latitude}
END

else

mapproject << END    ${R_options} ${J_options} -F  > corners.txt
${west_longitude} ${west_latitude}
${east_longitude} ${east_latitude}
END

fi

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



# Copy or create topography file
if [ "${earth_model}" = "null" ]
then
	echo 'using modern topography'
	cp ${root_directory}/${region}/topo/${region}.bin .
	cp ${nc_grid} ${region}.nc
	cp ${root_directory}/${region}/topo/elev_parameters.txt .
else

	echo "using deformed topography: ${gia_deformation}"
	awk -v time=${time} '{if($1*1000 == time) print $2, $3, $4}' ${root_directory}/deform/${gia_deformation} > gia.txt

	if [ "${special_projection}" = "y" ]
	then
		mapproject gia.txt  ${R_options} ${J_options_project} -F -C > gia_proj.xyz
	else
		mapproject gia.txt  ${R_options} ${J_options} -F  > gia_proj.xyz
	fi

#	blockmedian gia_proj.xyz -R${x_min}/${x_max}/${y_min}/${y_max} -I${spacing}=   -C  > gia_median.xyz

#	surface gia_median.xyz -Gdeform.nc -I${spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -T0.75 -V 

	triangulate gia_proj.xyz -bo -I5000 -R${x_min}/${x_max}/${y_min}/${y_max} -Gdeform.nc

	grdmath ${nc_grid} deform.nc SUB = ${region}.nc



	plot=deformed_base_topo.ps




	makecpt -Cglobe  > shades.cpt
	grdimage ${region}.nc ${shift_up}  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Cshades.cpt -V -nb > ${plot}

	pscoast -Bafg -O -K ${R_options} ${J_options} -P -Wthin -Di -A5000 -Wthin,black >> ${plot}

	makecpt -Cgray -T-500/1500/100    > deform.cpt
	grdcontour deform.nc -Cdeform.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0  -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

	mv ${plot} plots/

	grdconvert ${region}.nc ${region}.bin=bf 

	echo ${region}.bin > elev_parameters.txt
	echo ${x_min} >> elev_parameters.txt
	echo ${x_max} >> elev_parameters.txt
	echo ${y_min} >> elev_parameters.txt
	echo ${y_max} >> elev_parameters.txt
	echo ${spacing} >> elev_parameters.txt
	

fi







# create margin file for use in ice sheet
mkdir margins


if [ "${folder_on}" = "true" ]
then
   margin_file="${root_directory}/${region}/margins/${your_name}/${time}.gmt"
else
   margin_file="${root_directory}/${region}/margins/${time}.gmt"
fi

if [ -e "${margin_file}" ]
then
	cp ${margin_file} margins/
else
	echo "could not find margin for time: ${time}"
	echo "terminating...."
	exit 0
fi

# put into projected coordinates
if [ "${special_projection}" = "y" ]
then
echo "should be using new projected info"
mapproject margins/${time}.gmt  ${R_options} ${J_options_project} -F -C > margins/${time}_proj.gmt
else
echo "should not be using new projected info"

mapproject margins/${time}.gmt  ${R_options} ${J_options} -F  > margins/${time}_proj.gmt
fi

# split into multiple files

#awk '{print $0 > "margins/margin." NR}' RS='>'  ${time}_proj.gmt

awk '/>/{x=++i;next}{print > "margins/split_margin."x;}' margins/${time}_proj.gmt


reconstruction_folder=contours
mkdir ${reconstruction_folder}

thickness_dump_file=ice_thickness.txt

# create shear stress file

# TODO move the shear stress parameter files here
mkdir shear_stress

echo "${root_directory}/.." > shear_stress/root_directory


cp ${root_directory}/${region}/shear_stress/create_bin_var.sh shear_stress/create_bin.sh
cp ${root_directory}/${region}/shear_stress/shear_stress_domains.gmt shear_stress/
cp ${root_directory}/${region}/shear_stress/domains_max.txt shear_stress/
if [ "${adjust_file}" = "" ]
then
	echo "no adjustment to shear stress"
else
	cp ${root_directory}/${region}/shear_stress/${adjust_file} shear_stress/
	cp ${root_directory}/${region}/shear_stress/domains_min.txt shear_stress/
fi


cd shear_stress

bash create_bin.sh

mv shear_stress.bin ../
mv ss_parameters.txt ../

mv shear_stress.ps ../plots

cd .. 



log_file="icesheet_run.log"

counter=0

for margin_files in $( ls margins/split_margin.* )
do

counter=$( echo "${counter} + 1" | bc )

	cat << END_params > params.txt
${margin_files}
elev_parameters.txt
ss_parameters.txt
${icesheet_interval}
${icesheet_spacing}000
END_params

	echo "start time" >> ${log_file}
	date >> ${log_file}
	icesheet >> ${log_file}
	echo "end time" >> ${log_file}
	date >> ${log_file}



	if [ ! -e contours.txt ]
	then
		pwd
		echo "icesheet failed to run"
		exit 0
	fi

	cp contours.txt ${reconstruction_folder}/${counter}.contours
	rm contours-rejected.txt 

	gawk '{ if ($1 == ">" ){ } else {print $1, $2, $4}}' ${reconstruction_folder}/${counter}.contours >> ${thickness_dump_file}

	echo "finished: ${margin_files}"

done





coarse_spacing=40000 # just for plotting



grdmask margins/${time}_proj.gmt -I${spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -Gmask.nc
grdmask margins/${time}_proj.gmt -I${coarse_spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -Gmask_coarse.nc

blockmedian ${thickness_dump_file} -R${x_min}/${x_max}/${y_min}/${y_max} -I${spacing}=   -C  > reconstruction_thickness.txt

surface reconstruction_thickness.txt -Gice_thickness_raw.nc -I${spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -T0.25 -V 

grdmath ice_thickness_raw.nc mask.nc MUL = ice_thickness.nc


# make smoother contours
blockmedian ${thickness_dump_file} -R${x_min}/${x_max}/${y_min}/${y_max} -I${coarse_spacing}   -C  > reconstruction_thickness_coarse.txt

surface reconstruction_thickness_coarse.txt -Gice_thickness_raw_coarse.nc -I${coarse_spacing} -R${x_min}/${x_max}/${y_min}/${y_max} -T0.25 -V 

grdmath ice_thickness_raw_coarse.nc mask_coarse.nc MUL = ice_thickness_coarse.nc

# ice thickness plot

plot=ice_thickness.ps
makecpt -Cwysiwyg -T0/5000/250 > shades_ice.cpt

makecpt -Cgray -T0/4000/1000    > iceshades_coarse.cpt

grdimage ice_thickness.nc ${shift_up}  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Cshades_ice.cpt -V -nb > ${plot}

pscoast -Bafg -O -K ${R_options} ${J_options} -P -Wthin -Dl -A5000 -Wthin,grey >> ${plot}

grdcontour ice_thickness_coarse.nc -Ciceshades_coarse.cpt -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -O -W0.75p,black -A+f8p,black+gwhite >> ${plot}

psxy margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx1000f500+l"Ice Thickness (m)" --FONT_LABEL=14p -Cshades_ice.cpt -V  >> $plot

mv ${plot} plots/

# elevation plot

plot=ice_elevation.ps

grdmath ice_thickness.nc ${region}.nc ADD = ice_topo.nc

grdsample ice_topo.nc -Gice_topo_coarse.nc -I${coarse_spacing}

makecpt -Cjet -T-4000/4000/250  -I  > iceshades.cpt


makecpt -Cglobe -T-10000/10000 > shades.cpt
grdimage ${region}.nc ${shift_up}  -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 -K -P -Cshades.cpt -V -nb > ${plot}

pscoast -Bafg -O -K ${R_options} ${J_options} -P -Wthin -Di -A5000 -Wthin,black >> ${plot}

psclip margins/${time}_proj.gmt -K -O -R${x_min}/${x_max}/${y_min}/${y_max}  -JX${map_width}/0 >> $plot

grdimage ice_topo.nc -Ciceshades.cpt -J -R -V -P -nb+a+bg+t0.1 -K  -O >> ${plot}

grdcontour ice_topo_coarse.nc -Ciceshades_coarse.cpt -R -J -K -O -W+0.75p -A+f8p,black+gwhite >> ${plot}

psclip -K -O -C  >> $plot



psxy margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthickest,white >> ${plot}
psxy margins/${time}.gmt  ${R_options} ${J_options} -K -O -P -V -Wthin,blue >> ${plot}

psscale -X-1 -Y-3.5 -Dx9c/2c/9c/0.5ch -P -O -Bx1000f500+l"Ice elevation (m)" -G0/4000 -Ciceshades.cpt --FONT_LABEL=14p -V  >> $plot


mv ${plot} plots/


