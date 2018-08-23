	subroutine validationCurve(M, N, MV, Nlam, X, y, Xval, yval, lambda_vec, error_train, error_val)
	
	integer:: M, N, MV, Nlam
	double precision, dimension(M,N)::X
	double precision, dimension(M):: y
	double precision, dimension(N):: theta, grad
	double precision, dimension(MV, N):: Xval
	double precision, dimension(MV):: yval
	double precision, dimension(Nlam):: lambda_vec, error_train, error_val
	
	lambda_vec = (/0.0, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1., 3., 10.0/)
	
	 do i = 1, Nlam
         lambda = lambda_vec(i);         
		 call trainLinearReg(M, N, X, y, lambda, theta)
		 call  linearRegCostFunction(M, N, X, y, theta, lambda, error_train(i), grad)
		 call  linearRegCostFunction(MV, N, Xval, yval, theta, lambda, error_val(i), grad) 
      enddo
	   
	end subroutine validationCurve
