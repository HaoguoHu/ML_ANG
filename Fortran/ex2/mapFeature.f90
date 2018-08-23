	subroutine mapFeature(degree, X, M1, M, N, out)
	implicit none
	
	integer::degree, i, j, k, L=1, M, N, M1
	double precision,dimension(M, N)::out
	double precision,dimension(M, M1)::X
	
	do i=1,degree !only has 27 numbers 2~28
		do j=0,i
			L = L+1
		      do k=1, M
			  out(k, L) = X(k, 1)**(i-j) * X(k, 2)**j
			enddo
		enddo
	enddo
	
			do k=1, M  
			  out(k, 1) = 1  !1 more
			enddo
	end subroutine mapFeature
	
