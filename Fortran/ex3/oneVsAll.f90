	subroutine oneVsAll(all_theta)
	
	use mod_common
	
	implicit none
	
	integer:: k, j, i
	double precision,dimension(num_labels,N)::all_theta
	double precision,dimension(N,1)::theta
	double precision,dimension(M,1)::y
	
	do k=1, num_labels
	   write(*,*)'num_labels= ',  k
 
	  do i=1, M
	  	if(yy(i).eq. real(k)) then
		   y(i,1) = 1
		else
		   y(i,1) = 0
		endif
        enddo
	
	  theta = 0
	  
	  call fmincg(y,  theta)
	  
	  do j=1, N
		all_theta(k, j) = theta(j,1)
	  enddo
	  
	enddo
	
	
	end subroutine oneVsAll
