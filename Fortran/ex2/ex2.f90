	program ex2_AndrewNG
	implicit none
	
	integer,parameter :: m=100
	integer :: i, nfile=20
	double precision,dimension(2,m) :: X
	integer,dimension(m) :: y
	double precision,dimension(3) :: theta = 0,  grad, ini_theta=0.
	
!	character(1024) :: buffer
!  	character(20) :: var1, var2, var3
!  	integer :: pos

	double precision :: JCost = 0
   
	open(nfile, file='ex2data1.txt',status='old')
	do i = 1, m
!		read(nfile,"(A)") buffer
!  			pos = index(buffer, ",")
!  	 		var1 = buffer(1:pos-1)
!  		read(buffer(pos+1:), *) var2, var3
!  		write(*,*) var1, var2, var3  !For verification		
!		read(var1, * )X(1, i)
!		read(var2, * )X(2, i)
!		read(var3, * )y(i)

		read(nfile,*) X(1,i),X(2,i), y(i) !Now Fortran can read it directly
!		write(*,*) X(1,i),X(2,i), y(i) !For verification
	enddo
	close(nfile)
	
      call costFunction(m, ini_theta, X, y, JCost, grad)
	call fmincg(m, nn_params, input_layer_size, hidden_layer_size, num_labels, inputdata, y, lambda)
	
	write(*,*)
	write(*,*)'cost Function =', JCost
	write(*,*)
      write(*,*)'Gradient at initial theta (zeros)=', grad(1),grad(2),grad(3)
	write(*,*)
	
	end program ex2_AndrewNG
