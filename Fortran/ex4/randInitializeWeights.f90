	subroutine randInitializeWeights(L_in, L_out,  W)
    
	double precision:: epsilon_init = 0.12
	integer:: i, j, L_out, L_in
	
	double precision, dimension(L_out, L_in)::W
	
	call srand(1)
           
	do i=1, L_out  
	do j=1, L_in		
		W(i,j) = rand() * 2. * epsilon_init - epsilon_init		
	enddo
	enddo
	 
	end  subroutine randInitializeWeights


!   		  program test_rand
!            integer,parameter :: seed = 1
!          
!            call srand(seed)
!            print *, rand(), rand(), rand(), rand()
!            print *, rand(seed), rand(), rand(), rand()
!          end program test_rand
