!	copyright Evan. J. Gowan, 2013, 2016
!
!	This file is part of ICESHEET 1.0
!
!	ICESHEET is free software: you can redistribute it and/or modify
!	it under the terms of the GNU General Public License as published by
!	the Free Software Foundation, version 3 of the License
!
!	ICESHEET is distributed in the hope that it will be useful,
!	but WITHOUT ANY WARRANTY; without even the implied warranty of
!	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!	GNU General Public License for more details.
!
!	You should have received a copy of the GNU General Public License
!	along with ICESHEET.  If not, see <http://www.gnu.org/licenses/>.


subroutine flowline_location(interior_direction_temp, x_boundary, y_boundary, dx_flowline, dy_flowline, x_out, y_out)

	use global_parameters
	implicit none
	double precision, intent(in) :: interior_direction_temp, x_boundary, y_boundary, dx_flowline, dy_flowline
	double precision, intent(out) :: x_out, y_out

	double precision rot_x, rot_y, phi, vect, cart_dir, az_arg

	vect = sqrt(dx_flowline**2 + dy_flowline**2)

!	az_arg = -1./tan(interior_direction_temp)

!	cart_dir = atan2(az_arg,1.d0)

	cart_dir = -(interior_direction_temp - pi/2.)
	if (vect == 0.d0) THEN

		x_out = x_boundary
		y_out = y_boundary

	else

		phi = atan2( dy_flowline,dx_flowline)

!		rot_x = vect * cos(interior_direction_temp+phi)
!		rot_y = vect * sin(interior_direction_temp+phi)

!		rot_x = vect * cos(check_angle(cart_dir+phi))
!		rot_y = vect * sin(check_angle(cart_dir+phi))

		rot_x = dx_flowline * cos(cart_dir)
		rot_y = dx_flowline * sin(cart_dir)

		rot_x = rot_x + dy_flowline * cos(cart_dir+pi/2.)
		rot_y = rot_y + dy_flowline * sin(cart_dir+pi/2.)


!		if(interior_direction_temp > 0) THEN
!			rot_x = rot_x -dy_flowline * sin(check_angle(interior_direction_temp-pi/2.))
!			rot_y = rot_y + dy_flowline * cos(check_angle(interior_direction_temp-pi/2.))
!		else

!			rot_x = rot_x -dy_flowline * sin(check_angle(interior_direction_temp+pi/2.))
!			rot_y = rot_y + dy_flowline * cos(check_angle(interior_direction_temp+pi/2.))

!		endif

		x_out = rot_x + x_boundary
		y_out = rot_y + y_boundary

	!	write(3242,*) x_boundary, y_boundary, interior_direction_temp/ctr(), cart_dir/ctr(), dx_flowline, dy_flowline,&
	!		x_out, y_out, rot_x, rot_y
!		stop

	endif
!	write(741,*) x_out, y_out, dx_flowline, dy_flowline
!	write(66,*) "interior direction and phi", interior_direction_temp,phi
!	write(66,*) "initial x and y:", x_boundary, y_boundary
!	write(66,*) "dx and dy", dx_flowline, dy_flowline
!	write(66,*) "vect", vect
!	write(66,*) "output x and y:", x_out, y_out
end subroutine flowline_location
