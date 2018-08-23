
	subroutine learningCurve(M,N,MV, X, y, Xval, yval, lambda, error_train, error_val)

	integer:: i, j, k, nt
	double precision,dimension(M)::y, error_train, error_val
	double precision,dimension(M, N)::X
	double precision,dimension(MV)::yval
	double precision,dimension(MV, N)::Xval
	double precision,dimension(N)::theta, grad
	double precision::lambda, Jcost 
	
	double precision, allocatable::Xi(:, :)
	double precision, allocatable::yi(:)

	
	do i=1, M	
		allocate ( Xi(i, N), yi(i))		
		
		do j=1, i
			yi(j) = y(j)
			do k=1,N
				Xi(j,k) = X(j,k)
			enddo				
		enddo
	
		
		call trainLinearReg(i, N, Xi, yi, lambda, theta)	
		
		call  linearRegCostFunction(i, N, Xi, yi, theta, lambda, error_train(i), grad)
		call  linearRegCostFunction(MV, N, Xval, yval, theta, lambda, error_val(i),  grad)
		
		deallocate (Xi, yi)
		
	enddo

	end subroutine learningCurve
