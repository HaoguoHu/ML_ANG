	program ex3
	
	use mod_common
	
	implicit none

	integer::i,j, ta
	double precision,dimension(num_labels,N)::all_theta	
	double precision,dimension(M)::g
	
	open(20, file='ex3data1.txt', status='old')
	do i = 1, M
		read(20,*)(X(i,j),j=1,N), yy(i)
	enddo
	close(20)
	

	call oneVsAll(all_theta)
	
	call predictOneVsAll(all_theta, g);
	
	ta =0
	do i=1, M
!	  write(*,*) g(i), yy(i)
	  if(abs(yy(i)-g(i)).le.0.1)then
	    ta = ta +1
	   endif
	enddo
	
	write(*,*)'Train Accuracy: ', ta, M, real(ta)/real(M) * 100.
	
	end
