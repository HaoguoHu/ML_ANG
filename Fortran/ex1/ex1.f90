	program ex1_AndrewNG
	implicit none
	
	integer,parameter :: m=97, iterations = 1500
	integer :: i,iter,nfile=20
	real,dimension(m) :: X, y
	real,dimension(2) :: theta = 0
	
	character(1024) :: buffer
  	character(20) :: var1, var2
  	integer :: pos

	real :: alpha = 0.01
	real :: JCost = 0
      	real :: temp0, temp1
	real :: predict1=0, predict2=0
	
	integer:: N=0, k, j
	real,dimension(3,3)::xx, yy, zz


	open(nfile, file='ex1data1.txt',status='old')
	do i = 1, m
		read(nfile,"(A)") buffer
  			pos = index(buffer, ",")
  	 		var1 = buffer(1:pos-1)
  		read(buffer(pos+1:), *) var2
		
		read(var1, * )X(i)
		read(var2, * )y(i)
	enddo
	close(nfile)
	
	open(nfile, file='ex1data1_2.txt',status='unknown')
	do i = 1, m
		write(nfile, * )X(i), y(i)
	enddo
	close(nfile)

	open(nfile,file='cmddata1.txt', status='replace')
!	write(nfile,'(a)')'symbol(z) = "o+"[int(z):int(z)]'
	write(nfile,'(a)')'plot "ex1data1_2.txt" '  ! using 1:2:(symbol($2+1)) with labels textcolor lt 1 \'	
	write(nfile,'(a)')'q'
	close(nfile) 		
	call system('gnuplot -persist cmddata1.txt')
	
	call costFunction(M, theta, X, y, Jcost) 
	write(*,*)
	write(*,*)'ans =', JCost
	write(*,*)
	
        write(*,*)'Now, lets try Batch Gradient Descent'
	call batchGradientDescent(M, alpha, iterations, X, y, theta)
	    
	write(*,*)'Batch Gradient Descent with iterations 1500'
	write(*,*)'theta(1)=', theta(1), 'theta(2)=', theta(2)
	
	write(*,*)
	predict1 = 10000 * theta(1) + 35000 * theta(2)
	write(*,*)'For population = 35,000, we predict a profit of ', predict1
	
	predict2 = 10000 * theta(1) + 70000 * theta(2)
	write(*,*)'For population = 70,000, we predict a profit of ', predict2
	
	write(*,*)
	write(*,*)'Now, lets try stochasitic Gradient Descent'
		
	write(*,*)'Stochastic Gradient Descent solutions are:'
	write(*,*)'theta(1)=', theta(1), 'theta(2)=', theta(2)
	write(*,*)
	
	
	write(*,*)
	write(*,*)'Now, lets try the closed form-direct way'	
	call LMS(X, y, m, theta)	
	write(*,*)'The closed form solutions are:'
	write(*,*)'theta(1)=', theta(1), 'theta(2)=', theta(2)

	
	end program
