	program ex2_AndrewNG
	implicit none
	
	integer,parameter :: M1 =2, M = 118, ND=6, N=28  !ND: Degree of map features
	integer :: i, j, nfile=20, ta, iterations=400
	double precision,dimension(M, M1) :: X
	double precision,dimension(M, N) :: XM
	double precision,dimension(M) :: y, g
	double precision,dimension(N) :: theta=0,  grad	
	double precision :: JCost = 0, lambda = 1	
   
	open(nfile, file='ex2data2.txt',status='old')
	do i = 1, m
		read(nfile,*) X(i, 1),X(i, 2), y(i) !Now Fortran can read it directly
!		write(*,*) X(1,i),X(2,i), y(i) !For verification
	enddo
	close(nfile)
	
	call mapFeature(ND, X, M1, M, N, XM) !mapping X(M1, M) to XM(N, M)
      call costFunctionReg(M, N, XM, y, theta, JCost, grad, lambda)
	
	write(*,*)
	write(*,*)'cost Function =', JCost
	write(*,*)
	do i= 1, N
          write(*,'(A40,i2,f12.5)')'Gradient at initial theta (zeros)=', i,real(grad(i))
	enddo
	write(*,*)
	
	
	call fmincg(M, N, XM, y, theta)			
	call predict(M, N, XM, theta, g)	
	
	 ta =0;
	do i =1, M
!		   write(*,*) i, g(i), y(i);
	   if(abs(y(i) - g(i)) .lt. 0.49) then
	   	ta = ta +1
	    endif
	 enddo
	write(*,*)"Train Accuracy: ", ta, M, real(ta)/real(M) * 100.;
	
	
	end program ex2_AndrewNG
