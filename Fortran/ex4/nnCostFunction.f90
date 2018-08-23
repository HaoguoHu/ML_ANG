	subroutine nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, Jcost, grad)

	implicit none 
	
	integer::M, N, M1, N1, M2, N2
	double precision,dimension(M,N)::X
	double precision,dimension(M,N1)::X1
	double precision,dimension(M)::y
	double precision,dimension(M,N2)::sg
	double precision,dimension(M,M2)::sig, yy
	
	double precision,dimension(M)::g
	double precision,dimension(M1,N1)::theta1, grad1, delta1
	double precision,dimension(M2,N2)::theta2, grad2, delta2
	double precision::TX, max, sigmoid
	integer::i, k, j
	double precision::Jcost, lambda, atheta1, atheta2, sigmoidGradient
	
	double precision,dimension(N1):: a1
	double precision,dimension(N2):: a2,  z2t, tax, d2
	double precision,dimension(M2):: a3, yt, d3
	double precision,dimension(M1):: z2, d2t
	integer:: t
	double precision,dimension(M1*N1+M2*N2)::nn_params, grad

	call reshape(M1,N1,M2,N2,theta1,theta2,nn_params)
	
	do i=1,M
	   X1(i,1)=1
	   do j=2, N1
	   X1(i,j) = X(i,j-1)
	   enddo
	enddo
	
	do i=1, M 
	    do k=1, M2
		if(y(i) .eq. real(k))then
			yy(i,k)=1
		else
			yy(i,k)=0
		endif		  	    
	    enddo
	enddo

	
	do i=1, M
	do k=1, M1
	   	TX = 0
		do j=1, N1
		TX = TX + X1(i,j)*Theta1(k,j)
		enddo
		
		sg(i,k+1) = sigmoid(TX)
	enddo
	      sg(i,1) = 1
	enddo
	
	
	do i=1, M		 
	    do k=1, M2
	          TX = 0
		    do j=1, N2
			TX = TX + sg(i,j)* Theta2(k,j)
		    enddo
		    
		    sig(i, k) = sigmoid(TX)	    
		enddo								 
	enddo

	atheta1 =0
	do j=1, M1  
	do k=2, N1
		atheta1 = atheta1 + theta1(j,k) * theta1(j,k)
	enddo
	enddo
	 
	atheta2 =0 
	do j=1, M2 
	do k=2, N2  	
		atheta2 = atheta2 + theta2(j,k) * theta2(j,k)
	enddo
	enddo
	  
	   Jcost = 0	
	do i=1, M	        		  
	   do k=1, M2
        	Jcost = Jcost -yy(i,k) * log(sig(i,k)) - ( 1.-yy(i,k)) * log(1.-sig(i,k))
	   enddo     	     
	enddo	  
	
	Jcost = Jcost/M + lambda/2/M * (atheta1 + atheta2)
	  
!	  write(*,*) ' ok here 2'
	
	delta1 = 0
	delta2 = 0
	
 	
	do t=1, M
		do j=1,N1
		     a1(j) = X1(t,j)
		enddo
	
		do j=1,N2
		     a2(j) = sg(t,j)
		enddo
		
		do j=1,M2
		     	a3(j) = sig(t,j)
			yt(j) =  yy(t,j)
			d3(j) =  a3(j) - yt(j)
		enddo
		
		
		z2 = 0
		do j=1,M1
			do k=1,N1 
			z2(j) = z2(j) + theta1(j,k) *a1(k)				
			enddo
		enddo
	
		
			z2t(1) = 1
		do j=2,N2  
			z2t(j) = z2(j-1)
		enddo
	
	
		tax = 0		
		do j=1,N2		
			do k=1,M2 
			tax(j) = tax(j) + d3(k) * theta2(k,j)
			enddo
			d2(j) = tax(j) * sigmoidGradient(z2t(j))
		enddo

		
		do j=1,M1	
			d2t(j) = d2(j+1)
		enddo
		
		do i=1,M1
	     	do j=1,N1
		        delta1(i,j) = delta1(i,j) +  d2t(i)* a1(j)
		enddo
		enddo
	
		do i=1,M2
	     	do j=1,N2 
		     	delta2(i,j) = delta2(i,j) +  d3(i)* a2(j)
		enddo
		enddo		
	enddo
	
		
	do i=1,M1
		  grad1(i,1) = delta1(i,1)/M 
		do j=2,N1
	    	  grad1(i,j) = delta1(i,j)/M + lambda/M* theta1(i,j)
		enddo
	enddo
	
	do i=1,M2
		grad2(i,1) = delta2(i,1)/M 
	    do j=2,N2
	    	grad2(i,j) = delta2(i,j)/M + lambda/M* theta2(i,j)
	    enddo
	enddo


  	call unroll(M1,N1,M2,N2,grad1,grad2,grad)
		  	
	end subroutine nnCostFunction
