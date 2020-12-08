#! /bin/bash

# this script extracts ICE6G into xyz files for each of the times below, and it also removes the North American and Greenland ice which is calculated right now.
#
# The 122 ka ice thickness files can be found on W.R. Peltier's website (the file should be called ICE-6G_C_IceThickness_1deg.nc.gz)

# http://www.atmosp.physics.utoronto.ca/~peltier/data.php

counter=-1
for times in 122 120 118 116 114 112 110 108 106 104 102 100 98 96 94 92 90 88 86 84 82 80 78 76 74 72 70 68 66 64 62 60 58 56 54 52 50 48 46 44 42 40 38 36 34 32 31 30 29 28 27 26 25 24 23 22 21 20.5 20 19.5 19 18.5 18 17.5 17 16.5 16 15.5 15 14.75 14.5 14.25 14 13.75 13.5 13.25 13 12.75 12.5 12.25 12 11.75 11.5 11.25 11 10.75 10.5 10.25 10 9.75 9.5 9.25 9 8.75 8.5 8.25 8 7.5 7 6.5 6 5.5 5 4.5 4.25 4 3.75 3.5 3.25 3 2.75 2.5 2.25 2 1.75 1.5 1.25 1 0.75 0.5 0.25 0
do

counter=$(echo $counter | awk '{print $1+1}')

times_thousand=$( echo "${times}" | awk '{print $1 * 1000}')

echo ${times_thousand}

ncea -O -d Time,${counter},${counter} ICE-6G_C_IceThickness_1deg.nc test.nc

# North America and Greenland deleted
#grd2xyz test.nc?stgit | awk '{if ($2 > 0 && $1 > 180 && $1 < 347) {  print $1, $2, 0  } else {print $1, $2, $3}}' | awk '{if( $1 >300 && $2 > 65 || $1 == 360)  {  print $1, $2, 0  } else {print $1, $2, $3}  }'  > ${times_thousand}.xyz

# North America, Greenland and Eurasia deleted

grd2xyz test.nc?stgit | awk '{if ($2 > 0) {  print $1, $2, 0  } else {print $1, $2, $3}   }'  > ${times_thousand}.xyz

done

rm test.nc
