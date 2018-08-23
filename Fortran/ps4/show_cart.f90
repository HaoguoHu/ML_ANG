	!% This function displays the "animation"
	subroutine show_cart(x, x_dot, theta, theta_dot, pause_time)
	implicit none
	
	integer,parameter:: nf = 20
	real::x, x_dot, theta, theta_dot, pause_time
	real::length
	real, dimension(2):: plotx, ploty
	real, dimension(5):: xp, yp, xpt, ypt
	integer:: i, PGOPEN

	length = 3
	plotx(1) = x
	ploty(1) = 0
	
	plotx(2) = x + length * sin(theta)
	ploty(2) = length * cos(theta)
	
	xp(1) = x-0.4
	yp(1) = -0.25
	xp(2) = x+0.4
	yp(2) = -0.25
	xp(3) = x+0.4
	yp(3) = 0.
	xp(4) = x-0.4 
	yp(4) =  0.
	xp(5) =  x-0.4
	yp(5) = -0.25
	
	xpt(1) = x-0.01
	ypt(1) = -0.5
	xpt(2) = x+0.01
	ypt(2) = -0.5
	xpt(3) = x+0.01
	ypt(3) = 0.
	xpt(4) = x-0.01 
	ypt(4) =  0.
	xpt(5) =  x-0.01
	ypt(5) = -0.5
	

	IF (PGOPEN('/Xserve') .LT. 1) STOP
!	IF (PGOPEN('?') .LT. 1) STOP
!	call PGSCR(1,1,1,1)
	CALL PGENV(-3.,3.,-0.5,3.5,0,0)
    CALL PGLAB('(x)', '(y)', 'A Simple Graph')
	call PGLINE(2, plotx,ploty)
	call pgline(5, xp, yp)
	call pgline(5, xpt, ypt)
	call pgend

	call system('sleep 0.02')
	
	end subroutine show_cart
