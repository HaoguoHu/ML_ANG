	program ex3_nn
	
	use mod_common
	
	implicit none
	
	integer,parameter::M1=25, N1=401, M2=10, N2=26
!	double precision,dimension(M,N)::X
!	double precision,dimension(M)::yy	

	integer::i,j, ta
	double precision,dimension(M)::g	
	double precision,dimension(M1,N1)::Theta1
	double precision,dimension(M2,N2)::Theta2
	
	
	open(20, file='ex3data1.txt', status='old')
	do i = 1, M
		read(20,*)(X(i,j),j=1,N), yy(i)
	enddo
	close(20)
	
	open(20, file='ex3weights.txt', status='old')
	do i = 1, M1
		read(20,*)(Theta1(i,j),j=1,N1)
	enddo
	do i = 1, M2
		read(20,*)(Theta2(i,j),j=1,N2)
	enddo
	close(20)


	call predict( M1, N1, M2, N2, Theta1, Theta2, g)
	
	ta =0
	do i=1, M
	  write(*,*) g(i), yy(i)
	  if(abs(yy(i)-g(i)).le.0.1)then
	    ta = ta +1
	   endif
	enddo
	
	write(*,*)'Train Accuracy: ', ta, M, real(ta)/real(M) * 100.
	
	end
