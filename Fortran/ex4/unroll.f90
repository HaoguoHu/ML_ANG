	subroutine unroll(M1,N1,M2,N2,theta1,theta2,nn_params)
	integer:: i, j, k
	integer:: M1, N1, M2, N2
	double precision, dimension(M1,N1)::theta1
	double precision, dimension(M2,N2)::theta2
	double precision, dimension( M1*N1 + M2*N2)::nn_params

	k = 1
	do i=1,M1
	do j=1,N1 
		nn_params(k) = theta1(i,j)
		k = k + 1
	enddo
	enddo

	do i=1,M2
	do j=1,N2
		nn_params(k) = theta2(i,j)
		k = k + 1
	enddo
	enddo	

	end subroutine unroll
	
	
	subroutine reshape(M1,N1,M2,N2,theta1,theta2,nn_params)
	integer:: i, j, k
	integer:: M1, N1, M2, N2
	double precision, dimension(M1,N1)::theta1
	double precision, dimension(M2,N2)::theta2
	double precision, dimension( M1*N1 + M2*N2)::nn_params

  
	k = 1
	do i=1,M1
	do j=1,N1 
		theta1(i,j) = nn_params(k) 
		k = k + 1
	enddo
	enddo

	do i=1,M2
	do j=1,N2
	    theta2(i,j) = nn_params(k) 
		k = k + 1
	enddo
	enddo
	
	end subroutine reshape	
	


	
