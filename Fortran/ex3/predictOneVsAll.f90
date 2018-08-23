	subroutine predictOneVsAll(all_theta, g)
	use mod_common
	
	implicit none 
	double precision,dimension(M)::g
	double precision::sig
	double precision,dimension(num_labels,N)::all_theta
	double precision::TX, max, sigmoid
	integer::i, k, j
	
	do i=1, M
	       max = -1
		 g(i)= 0
		 
	      do k=1, num_labels
	          TX = 1
		    do j=1, N
			TX = TX + X(i,j)*all_theta(k,j)
		    enddo
		    
		    sig = sigmoid(TX)
		    
		    if(sig.gt.max)then
		       max = sig
			 g(i) = k
			 if(g(i).eq.0)g(i)=10.
		   endif
		    
		enddo
								 
	enddo

		
	 end subroutine predictOneVsAll
