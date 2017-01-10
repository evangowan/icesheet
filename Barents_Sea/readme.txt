Hopefully this setup is more straight forward to run and edit than the Greenland example. This is essentially the same
as the Barents Sea Ice Sheet example in the paper, except that modern Svalbard ice thickness has not been removed.

First, go into the shear_stress folder, and create the shear stress polygon file

 from QGIS:
 > Open shapefile (barents_sea_shear_stress.shp, in this folder)
 > Layer dropdown menu > save as
 > in the options, change "Symbology Export" to "feature symbology", and "Geometry" as "AS_WKT".
 > save file as "polygons.csv"



Look in the file "run.sh", and make sure the path to ICESHEET and the topography files are correct. You should be able
To modify the variables in run.sh for any problem.

type:

bash run.sh


It should work, and create plots of everything in the plots folder. The grid files are in the grids folder.
