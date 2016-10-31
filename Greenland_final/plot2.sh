#! /bin/bash


#simulation=20k_20m_20000a_gia0_mostlikely_0

simulation=$1



# The first time you run this, set to "y", so that it computes the averaged true Greenland ice thickness. Set to "n" after, it takes a lot of time to run!
execute_surface_mean=y

# make sure you change the path to the location of the Greenland dataset
greenland_netcdf=MCdataset-2015-04-27.nc


data_file=results/${simulation}_results.txt
ice_margin_file=outline.xyz 


FONT_ANNOT_PRIMARY_size=12p
FONT_LABEL_size=14p


x_interval=25000
y_interval=25000

#  region of interest, make sure it encompasses the provided outline at a minimum

x_min=-675000
x_max=895000

y_min=-3360000
y_max=-740000




xtext="x (m)"
xmin=${x_min}
xmax=${x_max}
xtickint=500000
xsubtickint=250000

ytext="y (m)"
ymin=${y_min}
ymax=${y_max}
ytickint=500000
ysubtickint=250000



if [ "${execute_surface_mean}" = "y" ]
then
ncks -v surface ${greenland_netcdf}  surface.nc

grd2xyz surface.nc -bo -sa -R${xmin}/${xmax}/${ymin}/${ymax} > greenland_surface.xyz

blockmean greenland_surface.xyz -bi -I${x_interval}/${y_interval} -R${x_min}/${x_max}/${y_min}/${y_max}  > greenland_surface_mean.xyz
fi


surface greenland_surface_mean.xyz -Ggreenland_surface_grid.nc -I${x_interval}=/${y_interval}= -R${x_min}/${x_max}/${y_min}/${y_max} -T0.35  -V 

# size of the x-y plot in cm

scale=0.8

# shift in the position of the plot from the bottom left corner in cm

xshift=1
yshift=5



map_width=8
map_height=$( echo  ${map_width} | awk  '{print $1*1.66875}' )


# plot elevation of the simulation and comparison with real surface


plot=plots/${simulation}_elev.ps
awk '{print $1, $2, $3}' ${data_file} > awk.out
#-Ba:/a:0wens
blockmean -R${x_min}/${x_max}/${y_min}/${y_max} -I${x_interval}=/${y_interval}= awk.out > mean_surface.out



surface mean_surface.out -Gelev_surface.grd  -I${x_interval}=/${y_interval}= -R${x_min}/${x_max}/${y_min}/${y_max} -T0.35  -V
#psbasemap  -X${xshift} -Y${yshift} -R${x_min}/${x_max}/${y_min}/${y_max} -Jb${center_xitude}/${center_yitude}/${southern_standard_parallel}/${northern_standard_parallel}/${map_width}  -Ba"${xtickint}"f"${xsubtickint}":"${xtext}":/a"${ytickint}"f"${ysubtickint}":"${ytext}":WeSn -V -P -K --MAP_TICK_LENGTH_PRIMARY=-.4c > $plot

R_options=${x_min}/${x_max}/${y_min}/${y_max}    #33.4/58.2/105/85r

Jx_options=${map_width}/${map_height}c


#Greenland / Arctic:
#Southernmost Latitude: 60° N
#Northernmost Latitude: 90° N
#Westernmost Longitude: 80° W
#Easternmost Longitude: 10° E

#Projection and Grid Description

#Polar Stereographic North (70° N, 45° W), corresponding to EPSG 3413.

#-56.3591133886	59.1040285668
#5.4155250852	79.3095523524

R_s_options="-56.3591133886/59.1040285668/5.4155250852/79.3095523524r" # this is based on the input x,y coordinates, calculated from qgis

JS_options="-45/90/70/${map_width}c" # projection of the Greenland data

# options for scale

Lf_options="-36.25/61/62/400+gwhite+l+jr"

psbasemap -X${xshift} -Y${yshift} -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -K --MAP_TICK_LENGTH_PRIMARY=-.4c > $plot

psclip ${ice_margin_file} -K -O -R -JX >> $plot

makecpt -Cjet -T-3500/3500/250 -G0/3500 -I  > iceshades.cpt

grdimage elev_surface.grd -R -JX -K -O  -Ciceshades.cpt  -V >> $plot


psclip -K -O -C >> $plot



#pscoast -R -JX -K -O -Dh -Na -V -A10000 -Wthick,black -Na/thick,black -K  >> $plot


psclip ${ice_margin_file} -K -O -R -JX >> $plot

grdcontour elev_surface.grd -Ciceshades.cpt -R -JX -K -O -W0.75p,darkblue -A+f8p,white >> ${plot}
#-A1000+f9p,black+gwhite

psclip -K -O -C >> $plot


psxy ${ice_margin_file} -K -O -R -JX -W1p,brown >> ${plot} 

psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

psscale  -X-5 -Y1.5 -G0/3500  -D9c/-2c/7c/0.5ch -O -K -Ba1000:"Ice surface elevation (m)": -Ciceshades.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size} >> $plot

psbasemap -X14 -Y-1.5  -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -K -O --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

psclip ${ice_margin_file} -K -O -R -JX >> $plot

makecpt -Cjet -T-500/500/100    > diff.cpt

grdmath greenland_surface_grid.nc elev_surface.grd SUB = compare.nc

grdimage compare.nc -R -JX -K -O  -Cdiff.cpt  -V >> $plot


psclip -K -O -C >> $plot



#pscoast -R -JX -K -O -Dh -Na -V -A10000 -Wthick,black -Na/thick,black -K  >> $plot


psclip ${ice_margin_file} -K -O -R -JX >> $plot

#grdcontour compare.nc -Cdiff.cpt -R -JX -K -O -W0.75p,darkblue -A500+f9p,black+gwhite >> ${plot}



psclip -K -O -C >> $plot

#psxy shear_stress/gmt_file.txt -JX -R -O -K -W1p,black >> ${plot}

psxy ${ice_margin_file} -K -O -R -JX -W1p,brown >> ${plot} 



psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

psscale  -X-5 -Y1.5   -D9c/-2c/7c/0.5ch -O -K -Ba500:"Difference elevation (m)": -Cdiff.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size} >> $plot


# plot the actual surface elevation and modelled surface elevation

plot=plots/greenland_surface.ps

psbasemap -X${xshift} -Y${yshift} -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -K --MAP_TICK_LENGTH_PRIMARY=-.4c > $plot

psclip ${ice_margin_file} -K -O -R -JX >> $plot

makecpt -Cjet -T-3500/3500/250 -G0/3500 -I  > iceshades.cpt

grdimage greenland_surface_grid.nc -R -JX -K -O  -Ciceshades.cpt  -V >> $plot


psclip -K -O -C >> $plot



#pscoast -R -JX -K -O -Dh -Na -V -A10000 -Wthick,black -Na/thick,black -K  >> $plot


psclip ${ice_margin_file} -K -O -R -JX >> $plot

grdcontour greenland_surface_grid.nc -Ciceshades.cpt -R -JX -K -O -W0.75p,darkblue -A+f9p,Helvetica-Bold,white  >> ${plot}
#-A1000+f9p,black+gwhite

psclip -K -O -C >> $plot






pscoast  -R${R_s_options} -JS${JS_options} -Dl -W1p,green -K -O >> ${plot}

psxy ${ice_margin_file} -K -O -R${R_options} -JX${Jx_options} -W1p,brown >> ${plot} 

psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

pstext << END_READ -R${R_s_options} -JS${JS_options} -F+f24p -K -O -Gwhite >> ${plot}
-54.3 60.5 (a)
END_READ


psscale   -X-5 -Y1.5 -G0/3500  -D9c/-2c/7c/0.5ch -O -K -Ba1000f500:"Measured elevation (m)": -Ciceshades.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size} >> $plot



psbasemap  -X14 -Y-1.5  -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -K -O --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

psclip ${ice_margin_file} -K -O -R -JX >> $plot

makecpt -Cjet -T-3500/3500/250 -G0/3500 -I  > iceshades.cpt

grdimage elev_surface.grd -R -JX -K -O  -Ciceshades.cpt  -V >> $plot


psclip -K -O -C >> $plot



#pscoast -R -JX -K -O -Dh -Na -V -A10000 -Wthick,black -Na/thick,black -K  >> $plot


psclip ${ice_margin_file} -K -O -R -JX >> $plot

grdcontour elev_surface.grd -Ciceshades.cpt -R -JX -K -O -W0.75p,darkblue -A+f9p,Helvetica-Bold,white  >> ${plot}
#-A1000+f9p,black+gwhite

psclip -K -O -C >> $plot



#psxy shear_stress/gmt_file.txt -JX -R -O -K -W0.5p,green >> ${plot}


pscoast  -R${R_s_options} -JS${JS_options} -Dl -W1p,green -K -O >> ${plot}


psxy ${ice_margin_file} -K -O -R${R_options} -JX${Jx_options} -W1p,brown >> ${plot} 

psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

pstext << END_READ -R${R_s_options} -JS${JS_options} -F+f24p -K -O -Gwhite >> ${plot}
-54.3 60.5 (b)
END_READ


psscale  -X-5 -Y1.5 -G0/3500  -D9c/-2c/7c/0.5ch -O -K -Ba1000f500:"Model elevation (m)": -Ciceshades.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size} >> $plot

# plot shear stress


plot=plots/shear_stress.ps

psbasemap -X${xshift} -Y${yshift} -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -K --MAP_TICK_LENGTH_PRIMARY=-.4c > $plot




makecpt -Cwysiwyg -T50/200/10  -I  > ss_shades.cpt

binary_ss_file="ss.bin"

grdmath ${binary_ss_file}=bf 1000 DIV = ss_k.nc

grdimage ss_k.nc -K -O -R -JX -Css_shades.cpt >> ${plot}

psxy shear_stress/gmt_file.txt -JX -R -O -K -W1p,black >> ${plot}
psxy ${ice_margin_file} -K -O -R -JX -W1p,brown -Ba:/a:0wens >> ${plot} 




psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot


pstext << END_READ -R${R_s_options} -JS${JS_options} -F+f24p -K -O -Gwhite >> ${plot}
-54.3 60.5 (c)
END_READ


psscale   -X-5 -Y1.5   -D9c/-2c/7c/0.5ch -O -K -Ba50:"Shear Stress (kPa)": -Css_shades.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size} >> $plot

psbasemap -X14 -Y-1.5  -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -K -O --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

psclip ${ice_margin_file} -K -O -R -JX >> $plot

makecpt -Cjet -T-500/500/100    > diff.cpt

grdmath greenland_surface_grid.nc elev_surface.grd SUB = compare.nc

grdimage compare.nc -R -JX -K -O  -Cdiff.cpt  -V >> $plot


psclip -K -O -C >> $plot


psclip ${ice_margin_file} -K -O -R -JX >> $plot



psclip -K -O -C >> $plot




pscoast  -R${R_s_options} -JS${JS_options} -Dl -W1p,green -K -O >> ${plot}

psxy ${ice_margin_file} -K -O -R${R_options} -JX${Jx_options} -W1p,brown >> ${plot} 



psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot


pstext << END_READ -R${R_s_options} -JS${JS_options} -F+f24p -K -O -Gwhite >> ${plot}
-54.3 60.5 (d)
END_READ


psscale  -X-5 -Y1.5   -D9c/-2c/7c/0.5ch -O -K -Ba200:"Measured - Model (m)": -Cdiff.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size} >> $plot




# plot ice thickness

plot=plots/${simulation}_thickness.ps
makecpt -Cjet -T0/3500/50   > shades.cpt
makecpt -Cjet -T0/3500/250   > shades_coarse.cpt

awk '{print $1, $2, $4}' ${data_file} > awk.out
#-Ba:/a:0wens
blockmean -R${x_min}/${x_max}/${y_min}/${y_max} -I${x_interval}=/${y_interval}= awk.out > mean.out
#map_width=0.75c # likely arbitrary


surface mean.out -Gthickness_surface.grd  -I${x_interval}=/${y_interval}= -R${x_min}/${x_max}/${y_min}/${y_max} -T0.35  -V

grdmask -I${x_interval}=/${y_interval}= -R${x_min}/${x_max}/${y_min}/${y_max} ${ice_margin_file} -Gmask.grd -N0/0/1



psbasemap -X${xshift} -Y${yshift} -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -K --MAP_TICK_LENGTH_PRIMARY=-.4c > $plot

psclip ${ice_margin_file} -K -O -R -JX >> $plot
grdimage thickness_surface.grd -R -JX -K -O  -Cshades.cpt  -V >> $plot



psclip -K -O -C >> $plot



psclip ${ice_margin_file} -K -O -R -JX >> $plot

grdcontour thickness_surface.grd -Cshades_coarse.cpt -R -JX -K -O -W0.75p,darkblue -A500+f9p,black+gwhite >> ${plot}

psclip -K -O -C >> $plot

psxy ${ice_margin_file} -K -O -R -JX -W2p,brown >> ${plot} 
psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

scale_style="9c/-2c/6c/0.5ch"

psscale   -X-4 -Y1.5     -D${scale_style} -O -K -Ba1000:"Ice thickness (m)": -Cshades.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size} >> $plot

#rm awk.out mean.out shades.cpt shades_coarse.cpt  elev_surface.grd thickness_surface.grd 

# comment out if you want to keep the yitude/xitude file

#rm ${y_x_file}
#ps2pdf -f ${plot}





# plot everything together




plot=plots/all_in_one_plot.ps

xshift=1
yshift=17.5

map_width=7
map_height=$( echo  ${map_width} | awk  '{print $1*1.66875}' )

R_options=${x_min}/${x_max}/${y_min}/${y_max}    #33.4/58.2/105/85r

Jx_options=${map_width}/${map_height}c

R_s_options="-56.3591133886/59.1040285668/5.4155250852/79.3095523524r" # this is based on the input x,y coordinates, calculated from qgis

JS_options="-45/90/70/${map_width}c" # projection of the Greenland data

# options for scale

Lf_options="-36.25/61/62/400+gwhite+l+jr"

# plot the actual surface elevation
psbasemap -X${xshift} -Y${yshift} -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -K --MAP_TICK_LENGTH_PRIMARY=-.4c > $plot

psclip ${ice_margin_file} -K -O -R -JX >> $plot

makecpt -Cjet -T-3500/3500/250 -G0/3500 -I  > iceshades.cpt

grdimage greenland_surface_grid.nc -R -JX -K -O  -Ciceshades.cpt  -V >> $plot


psclip -K -O -C >> $plot

psclip ${ice_margin_file} -K -O -R -JX >> $plot

grdcontour greenland_surface_grid.nc -Ciceshades.cpt -R -JX -K -O -W0.75p,darkblue -A+f9p,Helvetica-Bold,white  >> ${plot}

psclip -K -O -C >> $plot


pscoast  -R${R_s_options} -JS${JS_options} -Dl -W1p,green -K -O >> ${plot}

psxy ${ice_margin_file} -K -O -R${R_options} -JX${Jx_options} -W1p,brown >> ${plot} 

psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

letter_location="-54.3 60.7"

pstext << END_READ -R${R_s_options} -JS${JS_options} -F+f24p -K -O -Gwhite >> ${plot}
${letter_location} (a)
END_READ

scale_xshift=-5.5
scale_yshift=1.5
psscale   -X${scale_xshift} -Y${scale_yshift} -G0/3500  -D${scale_style} -O -K -Ba1000f500:"Measured elevation (m)": -Ciceshades.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size} --MAP_LABEL_OFFSET=5p >> $plot

xshift=$( echo $map_width ${scale_xshift} 1 | awk '{print $1 - $2 + $3}' )
# plot modelled surface elevation
echo ${xshift} ${xshift} ${xshift} ${xshift}
psbasemap  -X${xshift} -Y-${scale_yshift}  -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -K -O --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

psclip ${ice_margin_file} -K -O -R -JX >> $plot

makecpt -Cjet -T-3500/3500/250 -G0/3500 -I  > iceshades.cpt

grdimage elev_surface.grd -R -JX -K -O  -Ciceshades.cpt  -V >> $plot


psclip -K -O -C >> $plot



psclip ${ice_margin_file} -K -O -R -JX >> $plot

grdcontour elev_surface.grd -Ciceshades.cpt -R -JX -K -O -W0.75p,darkblue -A+f9p,Helvetica-Bold,white  >> ${plot}


psclip -K -O -C >> $plot




pscoast  -R${R_s_options} -JS${JS_options} -Dl -W1p,green -K -O >> ${plot}


psxy ${ice_margin_file} -K -O -R${R_options} -JX${Jx_options} -W1p,brown >> ${plot} 

psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

pstext << END_READ -R${R_s_options} -JS${JS_options} -F+f24p -K -O -Gwhite >> ${plot}
${letter_location}  (b)
END_READ


psscale  -X${scale_xshift} -Y${scale_yshift} -G0/3500  -D${scale_style} -O -K -Ba1000f500:"Model elevation (m)": -Ciceshades.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size} --MAP_LABEL_OFFSET=5p >> $plot



yshift=$( echo 14.5 ${scale_yshift} | awk '{print -$1 - $2}' )
xshift=$( echo $map_width ${scale_xshift} 1 | awk '{print  -$1 - $2 - $3}' )

echo ${xshift} ${xshift} ${xshift} ${xshift}

psbasemap -X${xshift} -Y${yshift} -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -O -K --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot




makecpt -Cwysiwyg -T50/200/10  -I  > ss_shades.cpt

binary_ss_file="ss.bin"

grdmath ${binary_ss_file}=bf 1000 DIV = ss_k.nc

grdimage ss_k.nc -K -O -R -JX -Css_shades.cpt >> ${plot}

psxy shear_stress/gmt_file.txt -JX -R -O -K -W1p,black >> ${plot}
psxy ${ice_margin_file} -K -O -R -JX -W1p,brown -Ba:/a:0wens >> ${plot} 

psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot


pstext << END_READ -R${R_s_options} -JS${JS_options} -F+f24p -K -O -Gwhite >> ${plot}
${letter_location}  (c)
END_READ


psscale  -X${scale_xshift} -Y${scale_yshift}   -D${scale_style} -O -K -Ba50:"Basal Shear Stress (kPa)": -Css_shades.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size}  --MAP_LABEL_OFFSET=5p >> $plot

xshift=$( echo $map_width ${scale_xshift} 1 | awk '{print $1 - $2 + $3}' )

psbasemap  -X${xshift} -Y-${scale_yshift}   -R${R_options} -JX${Jx_options} -Ba:/a:0wens -V -P -K -O --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot

psclip ${ice_margin_file} -K -O -R -JX >> $plot

makecpt -Cjet -T-500/500/100    > diff.cpt

grdmath greenland_surface_grid.nc elev_surface.grd SUB = compare.nc

grdimage compare.nc -R -JX -K -O  -Cdiff.cpt  -V >> $plot

grdmask ${ice_margin_file} -Gclipping_mask.nc -R  -I${x_interval}=/${y_interval}= -NNan/NaN/1

grdmath compare.nc clipping_mask.nc MUL = compare2.nc

grd2xyz compare2.nc -R -s > compare_greenland.xyz

psclip -K -O -C >> $plot



psclip ${ice_margin_file} -K -O -R -JX >> $plot


psclip -K -O -C >> $plot

pscoast  -R${R_s_options} -JS${JS_options} -Dl -W1p,green -K -O >> ${plot}

psxy ${ice_margin_file} -K -O -R${R_options} -JX${Jx_options} -W1p,brown >> ${plot} 



psbasemap   -R${R_s_options} -JS${JS_options}   -Ba:/a:0wens -V -P -K -O -Lf${Lf_options} --MAP_TICK_LENGTH_PRIMARY=-.4c >> $plot


pstext << END_READ -R${R_s_options} -JS${JS_options} -F+f24p -K -O -Gwhite >> ${plot}
${letter_location}  (d)
END_READ


psscale  -X${scale_xshift} -Y${scale_yshift}   -D${scale_style} -O -K -Ba200:"Measured - Model (m)": -Cdiff.cpt --FONT_ANNOT_PRIMARY=${FONT_ANNOT_PRIMARY_size}  --FONT_LABEL=${FONT_LABEL_size} --MAP_LABEL_OFFSET=5p >> $plot


