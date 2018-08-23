	program ex4
	
	implicit none
	
	integer,parameter::M=5000, N=400, M1=25, N1=401, M2=10, N2=26
	double precision,dimension(M,N)::X
	double precision,dimension(M)::y	

	integer::i,j, ta
	double precision,dimension(M)::g	
	double precision,dimension(M1,N1)::theta1, grad1, initial_theta1
	double precision,dimension(M2,N2)::theta2, grad2, initial_theta2
	double precision::Jcost, lambda, sigmoidGradient
	double precision,dimension(5)::Z
	
	double precision,dimension(M1*N1+M2*N2)::nn_params, grad
	
	
	open(20, file='ex4data1.txt', status='old')
	do i = 1, M
		read(20,*)(X(i,j),j=1,N), y(i)
	enddo
	close(20)
	
	open(20, file='ex4weights.txt', status='old')
	do i = 1, M1
		read(20,*)(theta1(i,j),j=1,N1)
	enddo
	do i = 1, M2
		read(20,*)(theta2(i,j),j=1,N2)
	enddo
	close(20)

	call unroll(M1,N1,M2,N2,theta1,theta2,nn_params)
		
	lambda = 0.
	call nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, Jcost, grad)
	write(*,*)'Jcost=', Jcost
	write(*,*)'this value should be about 0.287629'
	write(*,*)
	
	
	lambda = 1.
	call nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, Jcost, grad)
	write(*,'(A6,f9.6)')'Jcost=', Jcost
	write(*,*)'this value should be about 0.383770'
	write(*,*)
	
	
	Z =(/1., -0.5, 0., 0.5, 1./)
	write(*,*)'Sigmoid gradient evaluated at [1 -0.5 0 0.5 1]: '
	write(*,'(5f9.6)') (sigmoidGradient(z(i)),i=1,5)
	write(*,*)
	
	write(*,*)'Checking Backpropagation...'
	lambda =0;
	call checkNNGradients(lambda)
	write(*,*)

	write(*,*)'Checking Backpropagation (w/ Regularization) ... '
	lambda = 3;
	call checkNNGradients(lambda)
	write(*,*)

	call nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, Jcost, grad)
	write(*,'(A6,f9.6)')'Jcost=', Jcost
	write(*,*)'this value should be about 0.576051'
	write(*,*)
	
	call randInitializeWeights(N1, M1, initial_theta1)
	call randInitializeWeights(N2, M2, initial_theta2)
	
	call unroll(M1,N1,M2,N2,initial_theta1, initial_theta2, nn_params)

	lambda = 1.
	call fmincg(M,N,M1,N1,M2,N2, nn_params,X, y,lambda, 50)
	call reshape(M1,N1,M2,N2,theta1, theta2, nn_params)
	
	call predict(M, N, M1, N1, M2, N2, X, theta1, theta2, g)
	ta =0
	do i=1, M
!	  write(*,*) g(i), y(i)
	  if(abs(y(i)-g(i)).le.0.1)then
	    ta = ta +1
	   endif
	enddo
	
	write(*,*)'Train Accuracy: ', ta, M, real(ta)/real(M) * 100.
	
	end
