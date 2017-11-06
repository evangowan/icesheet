FC = gfortran

FCFLAGS = -g -fbacktrace -fbounds-check 
#FCFLAGS = -O2

# if compiling with the Intel Fortran compiler, you need to add an extra flag
#FCFLAGS = -O2 -assume byterecl

objfiles =  global_parameters.o grids.o read_icefile.o  find_flowline_fisher_adaptive_4.o flowline_location.o 



icesheet: icesheet.f90 $(objfiles)
	$(FC) -o icesheet $(FCFLAGS) icesheet.f90 $(objfiles)

global_parameters.o: global_parameters.f90
	$(FC) -o global_parameters.o $(FCFLAGS) -c global_parameters.f90

grids.o: grids.f90
	$(FC) -o grids.o $(FCFLAGS) -c grids.f90

read_icefile.o: read_icefile.f90
	$(FC) -o read_icefile.o $(FCFLAGS) -c read_icefile.f90

find_flowline_fisher_adaptive_4.o: find_flowline_fisher_adaptive_4.f90
	$(FC) -o find_flowline_fisher_adaptive_4.o $(FCFLAGS) -c find_flowline_fisher_adaptive_4.f90

flowline_location.o: flowline_location.f90
	$(FC) -o flowline_location.o $(FCFLAGS) -c flowline_location.f90



read_polygons.o: read_polygons.f90
	$(FC) -o read_polygons.o $(FCFLAGS) -c read_polygons.f90


#####################

nearest_int: nearest_int.f90 bicubic.o
	$(FC) -o nearest_int  $(FCFLAGS2) nearest_int.f90 bicubic.o

bicubic.o: bicubic.f90
	$(FC) -o bicubic.o -c  $(FCFLAGS2) bicubic.f90



#####################


read_dem.o: read_dem.f90
	$(FC) -o read_dem.o -c  $(FCFLAGS2) read_dem.f90

reduce_dem: reduce_dem.f90 read_dem.o
	$(FC) -o reduce_dem  $(FCFLAGS2) reduce_dem.f90 read_dem.o


#####################


create_ss_grid: create_ss_grid.f90 global_parameters.o
	$(FC) -o create_ss_grid $(FCFLAGS) create_ss_grid.f90 global_parameters.o


####################

no_ice: no_ice.f90 global_parameters.o
	$(FC) -o no_ice $(FCFLAGS) no_ice.f90 global_parameters.o
