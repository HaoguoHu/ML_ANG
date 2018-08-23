subroutine checkNNGradients(lambda)
	implicit none
	
!	N =input_layer_size =3 
!	N1=input_layer_size+1 =4
!	M1=hidden_layer_size =5
!	M2=num_labels =3
!	N2=hidden_layer_size+1 =6

	integer, parameter::M = 5, N=3, N1=4, M1=5, M2=3, N2=6
	double precision, dimension(M,N):: X
	double precision, dimension(M):: y
	
	double precision, dimension(M1,N1):: grad1, theta1  
	double precision, dimension(M2,N2):: grad2, theta2
	integer::i, j, nn
	double precision, dimension(M1*N1+M2*N2):: numgrad,nn_params, grad
	double precision:: diff, tmp, lambda, Jcost
		
	call debugInitializeWeights(M1, N1-1, theta1)
	call debugInitializeWeights(M2, N2-1, theta2)     
	call debugInitializeWeights(M, N-1, X);


	do i=1, M
		y(i) = 1. + real(mod((i-1), M2))
	enddo

	call unroll(M1,N1,M2,N2,theta1,theta2,nn_params)
    call nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, Jcost, grad)	
	call computeNumericalGradient(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, numgrad)


	diff = -1.
	   
	do i=1, M1*N1+M2*N2		
			write(*,*)i, numgrad(i), grad(i)
			tmp = (numgrad(i)-grad(i))/(numgrad(i)+grad(i))
			if(tmp .gt. diff)then
				diff = tmp
			endif
	enddo	
	
	write(*,*)'The above two columns you get should be very similar.'
    write(*,*)'(Left-Your Numerical Gradient, Right-Analytical Gradient)'
!	read(*,*)

	write(*,*)'If your backpropagation implementation is correct, then'
    write(*,*)'the relative difference will be small (less than 1e-9)'
    write(*,'(A30,e12.5)')'Relative Difference:', diff

end subroutine checkNNGradients
