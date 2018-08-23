	subroutine predict(M, N, X, theta, gg)

	implicit none 
	
	integer::M, N
	double precision,dimension(M,N)::X	
	double precision,dimension(M)::gg
	double precision,dimension(N)::theta
	double precision::TX, sigmoid
	integer::i, k, j
	
	
	do i=1, M	
	
	          TX = 0
		    do j=1, N
			TX = TX + X(i,j)*theta(j)
		    enddo
		    
		    gg(i) = sigmoid(TX)	
		    	 				 
	enddo

	
		
	 end subroutine predict
