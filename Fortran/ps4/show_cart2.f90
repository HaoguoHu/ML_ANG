	!% This function displays the "animation"
	subroutine show_cart(x, x_dot, theta, theta_dot, pause_time)
	implicit none
	
	
	double precision::x, x_dot, theta, theta_dot, pause_time
	double precision::length
	double precision, dimension(2):: plotx, ploty
	integer::nf = 20, i

	length = 3
	plotx(1) = x
	ploty(1) = 0
	
	plotx(2) = x + length * sin(theta)
	ploty(2) = length * cos(theta)
	
	open(nf, file='ps4.txt',status='replace')
	do i=1, 2
		write(nf,*)plotx(i), ploty(i)
	enddo
		write(nf,*)
		write(nf,*)x-0.4,  -0.2
		write(nf,*)x+0.85, -0.2
		write(nf,*)x+0.85, 0.25
		write(nf,*)x-0.4,  0.25
		write(nf,*)x-0.4, -0.25			
		write(nf,*)
		write(nf,*)x-0.01, -0.5
		write(nf,*)x+0.02, -0.5
		write(nf,*)x+0.02, 0.25
		write(nf,*)x-0.01, 0.25
		write(nf,*)x-0.01, -0.5	
	close(nf)
	
!	open(nf,file='cmdps4.txt', status='replace')
!! 		write(nf,'(a)')'rgb(r,g,b) = 65536 * int(r) + 256 * int(g) + int(b)'
!		write(nf,'(a)')'set xrange[-3:3]'
!		write(nf,'(a)')'set yrange[-0.5:3.5]'	
!		write(nf,'(a)')'plot "ps4.txt" using 1:2 with line'		
!		write(nf,'(a)')'q'
!	close(nf) 		
	call system('gnuplot -persist cmdps4.txt')
!	call system('sleep 0.02')
	stop

	end subroutine show_cart
