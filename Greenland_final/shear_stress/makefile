FC = gfortran

FCFLAGS = -g -fbacktrace -fbounds-check 
FCFLAGS = -O2

create_ss_grid: create_ss_grid.f90 
	$(FC) -o create_ss_grid $(FCFLAGS) create_ss_grid.f90 

convert_grid: convert_grid.f90 
	$(FC) -o convert_grid $(FCFLAGS) convert_grid.f90 


