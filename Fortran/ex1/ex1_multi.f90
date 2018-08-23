	program ex1_AndrewNG
	implicit none
	
	integer,parameter :: M=47, N=3, iterations = 50
	integer :: i,iter,nfile=20
	real,dimension(N-1, m) :: X, X_norm
	real,dimension(m) :: y
	real,dimension(N) :: theta = 0
	real,dimension(N-1) :: mu, sigma
		
	real :: alpha = 0.15
	real :: JCost = 0
      real :: temp0, temp1
	real :: predict
	
	integer::   j
	
	real,dimension(M,N) :: XMN 
	real,dimension(N,N) :: XNN, Xi 
	real,dimension(N,M) :: XNM, XT		

	open(nfile, file='ex1data2.txt',status='old')
	do i = 1, m		
  		read(nfile,*) X(1,i),X(2,i), y(i)
!		write(*,*) X(1,i),X(2,i), y(i) !For verification
	enddo
	close(nfile)

	call featureNormalize(M, X, X_norm, mu, sigma)
	 write(*,*)mu
	 write(*,*)sigma
	 do i=1,10
	   write(*,*)X_norm(1,i), X_norm(2, i)
	 enddo
      
	   write(*,*)'Now, lets try Batch Gradient Descent'
	call batchGradientDescentMulti(M, 3, alpha, iterations, X_norm, y, theta)	    
	write(*,*)'Batch Gradient Descent with iterations 50'
	write(*,*)'theta(1)=', theta(1), 'theta(2)=', theta(2), 'theta(3)=', theta(3)
	
	
	write(*,*)
	write(*,*)'Now, lets Estimate the price of a 1650 sq-ft, 3 br house'	
	predict = theta(1) + (1650 - mu(1))/sigma(1)*theta(2) +(3-mu(2))/sigma(2)*theta(3) 		
	write(*,*) predict		
	write(*,*)

	write(*,*)'Now, lets Solving with normal equations..'		

	   XMN(:,1) = 1
	   do j = 2, N
	      XMN(:,j) = X(j-1, :)
	   enddo
	 
	 call transpose(XMN, M, N, XT)
	 call multiply(XT, N, M, N, XMN, XNN)
	 call inverse(XNN, Xi, N)
	 call multiply(Xi, N, N, M, XT, XNM)
	 call multiply(XNM, N, M, 1, Y, theta)
	 
	write(*,*)'Theta computed from the normal equations'
	write(*,*) theta
	write(*,*)
	

	
	end program
