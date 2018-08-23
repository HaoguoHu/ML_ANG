	subroutine predict(M, N, M1, N1, M2, N2, X, Theta1, Theta2, g)

	implicit none 
	
	integer::M, N, M1, N1, M2, N2
	double precision,dimension(M,N)::X
	double precision,dimension(M,N1)::X1
	double precision,dimension(M,N2)::gg
	
	double precision,dimension(M)::g
	double precision::sig
	double precision,dimension(M1,N1)::Theta1
	double precision,dimension(M2,N2)::Theta2
	double precision::TX, max, sigmoid
	integer::i, k, j
	
	
	do i=1,M
	   X1(i,1)=1
	   do j=2, N1
	   	X1(i,j) = X(i,j-1)
	   enddo
	enddo
	

	
	do i=1, M
	   do k=1, M1
	   	TX = 0;
		do j=1, N1
			TX = TX + X1(i,j)*Theta1(k,j)
		enddo
		
		gg(i,k+1) = sigmoid(TX)
	    enddo
	      gg(i,1) = 1
	 enddo
		 
!	write(*,*) ' ok here 2'
	
	
	do i=1, M
	       max = -1
		 
	      do k=1, M2
	          TX = 0
		    do j=1, N2
			TX = TX + gg(i,j)* Theta2(k,j)
		    enddo
		    
		    sig = sigmoid(TX)
		    
		    if(sig.gt.max)then
		       max = sig
			 g(i) = k
			 if(g(i).eq.0)g(i)=10.
		   endif
		    
		enddo
								 
	enddo

		
	 end subroutine predict
