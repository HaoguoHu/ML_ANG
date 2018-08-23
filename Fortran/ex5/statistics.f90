function mean(M, X)

	integer::i, M
	double precision:: X(M)
	double precision:: mean
	
		mean = 0
	do i=1, M
		mean = mean + X(i)
	enddo
		mean = mean/M 
	
end function mean	


subroutine meanN(M, N,  X, y)

	integer::i, j, M, N
	double precision X(M, N), y(N)
	
	do j=1, N
		y(j) = 0
		do i=1, M
		y(j) = y(j) + X(i, j)
		enddo
		y(j) = y(j)/M
	enddo
	
end subroutine meanN	
		


subroutine std(M, N, X, sigma)
	integer:: i, j, M, N
	double precision, dimension(M,N):: X
	double precision, dimension(N)::y, sigma
	double precision:: sum 
	
	call meanN(M, N, X, y)
	
	do j=1, N
			sigma(j) = 0
		do i=1, M
			sigma(j) = sigma(j) + (X(i,j) - y(j))**2
		enddo
		
		sigma(j) = sqrt(sigma(j)/(M-1))
	enddo
end subroutine std		
	


subroutine bsxfunM(M, N, X, y)

	integer:: i, j, M, N
	double precision::X(M,N), y(M)
	
	do j=1, N
		do i=1, M
		X(i,j) = X(i,j) - y(j)
		enddo
	enddo
end subroutine bsxfunM
		


subroutine bsxfunP(M, N, X, y)

	integer:: i, j, M, N
	double precision::X(M,N), y(M)
	
	do j=1, N 
		do i=1, M		
		X(i,j) = X(i,j) + y(j)
		enddo
	enddo
end subroutine bsxfunP



subroutine bsxfunRD(M, N, X, y)

	integer:: i, j, M, N
	double precision::X(M,N), y(M)
	
	
	do j=1, N 
		do i=1, M
		X(i,j) = X(i,j) / y(j)
		enddo
	enddo
end subroutine bsxfunRD



subroutine bsxfunLD(M, N, X, y)

	integer:: i, j, M, N
	double precision::X(M,N), y(M)
	
	do i=1, M
		do j=1, N 
		X(i,j) = y (j) / X(i,j)
		enddo
	enddo
end subroutine bsxfunLD



subroutine zeros( M, N, X, X1)

	integer:: i, j, M, N
	double precision::X(M,N), X1(M, N+1)
	
	do i=1, M
		X1(i,1) = 0
		do j=2, N+1 
		X1(i,j) = X(i,j-1) 
		enddo
	enddo
end subroutine zeros	



subroutine ones( M, N, X, X1)

	integer:: i, j, M, N
	double precision::X(M,N), X1(M, N+1)
	
	do i=1, M
		X1(i,1) = 1
		do j=2, N+1 
		X1(i,j) = X(i,j-1) 
		enddo
	enddo
end subroutine ones	

