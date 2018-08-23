
	subroutine polyFeatures(M, X, N, Xp)

	integer:: i, j;
	integer::M, N
	double precision::X(M), Xp(M, N)
	
	
	do i=1, M 	
	do j=1, N 
		Xp(i ,j) = X(i)**(j)
	enddo
	enddo
	
	

	end subroutine polyFeatures
