	program ex5
	implicit none
	
	
	integer, parameter::M=12, N=2, MV=21, p=8, Nlam=10
	double precision,dimension(M)::X, y, error_train, error_val
	double precision,dimension(M, N)::X1
	double precision,dimension(MV)::Xval, yval, Xtest
	double precision,dimension(MV, N)::Xval1
	double precision,dimension(N)::theta, grad
	double precision::lambda, Jcost 
	integer::i,j
	
	
	double precision,dimension(M, p):: X_poly
	double precision,dimension(M, p+1):: X_poly1
	double precision,dimension(p):: mu, sigma	
	
	double precision:: X_poly_test(MV,p), X_poly_test1(MV, p+1) 
	double precision::  X_poly_val(MV,p), X_poly_val1(MV, p+1) 	
	
	double precision, dimension(Nlam):: lambda_vec, error_trainV, error_valV		
	
	open(20, file='ex5data1.txt', status='old')
	do i = 1, M
		read(20,*)X(i), y(i)
!		write(*,*)X(i), y(i)
	enddo
	do i=1, MV
		read(20,*)Xval(i), yval(i), Xtest(i)
	enddo
	close(20)
	
	theta = 1.
	X1(:, 1) = 1.
	X1(:, 2) = X(:)
	
	lambda = 1
	call linearRegCostFunction(M, N, X1, y, theta, lambda, Jcost, grad)
	
	
	write(*,*)'Jcost = ', Jcost
	write(*,*)'(this value should be about 303.993192)'
	write(*,*)
	
	
	write(*,*)'Gradient at theta = [1  1]:', grad(1), grad(2)
	write(*,*)'(this value should be about [-15.303016 598.250744])'
	write(*,*)
	
	
	lambda = 0
	call trainLinearReg(M, N, X1, y, lambda, theta)
	write(*,*)'theta= ', theta
	write(*,*)'(theta should be about  13.087904 ,0.367779 )'
	write(*,*)
	
	
	
		lambda = 0
		Xval1(:,1) = 1
		Xval1(:,2) = Xval(:)		
	call learningCurve(M, N, MV, X1, y, Xval1, yval, lambda, error_train, error_val)	
	write(*,*)'# Training Examples Train Error Cross Validation Error'	
	do i=1,M
		write(*,'(i6,2f20.6)') i, error_train(i),  error_val(i)
	enddo
	write(*,*)
	write(*,*)
	
	
	
!  Map X onto Polynomial Features and Normalize	
	call polyFeatures(M, X, p, X_poly)				
	call featureNormalize(M, p, X_poly, mu, sigma)		 
	call ones(M, p, X_poly, X_poly1)	
	

!// Map X_poly_test and normalize (using mu and sigma)	
	call polyFeatures(MV, Xtest, p, X_poly_test)	
	call bsxfunM( MV, p, X_poly_test, mu)								
	call bsxfunRD(MV, p, X_poly_test, sigma)	
	call ones(MV, p, X_poly_test, X_poly_test1)



!// Map X_poly_val and normalize (using mu and sigma)	
	call polyFeatures(MV, Xval, p, X_poly_val)
	call bsxfunM( MV, p, X_poly_val, mu);								
	call bsxfunRD(MV, p, X_poly_val, sigma);
	call ones(MV, p, X_poly_val, X_poly_val1)	
	
	
	write(*,*)'Normalized Training Example 1'
	do j=1,p+1
	write(*,'(f20.6)')X_poly1(1, j)
	enddo
	write(*,*)
	
	
	lambda = 0
	call trainLinearReg(M, p+1, X_poly1, y, lambda, theta)	
	call learningCurve(M, p+1, MV, X_poly1, y, X_poly_val1, yval, lambda, error_train, error_val)

	write(*,*)
	write(*,*)
	write(*,*)'Polynomial Regression: lambda =', lambda
	write(*,*)'# Training Examples    Train Error    Cross Validation Error'
	do i=1, M 
		write(*,'(i4, 2f20.6)') i, error_train(i), error_val(i)
	enddo		
	write(*,*)'Program paused. Press enter to continue.'
	read(*,*)
	
	
	call validationCurve(M, p+1, MV, Nlam,  X_poly1, y, X_poly_val1, yval, lambda_vec, error_trainV, error_valV)
	write(*,*)'		lambda_vec	error_train	  error_val'
	do i=1, Nlam 
	write(*,'(i4, 3f20.6)')i, lambda_vec(i), error_train(i), error_val(i)
	enddo

	end
	
	
