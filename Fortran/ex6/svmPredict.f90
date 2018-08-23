	subroutine svmPredict(M, N, idx, model, X, pred, kernelFunction)
	implicit none
	
	
	integer::M, N , idx, i, j, M1
	double precision,dimension(M,N)::X, XX
	double precision,dimension(M)::p, pred, X1
	
	double precision,dimension(M,idx)::K
	double precision,dimension(idx)::X2
	double precision,dimension(idx, N)::XM
	
	double precision,dimension(1)::x11, x12	
	
	character(len=20)::kernelFunction
	double precision:: gaussianKernel, sigma
	
	type::modelL
		double precision, allocatable, dimension(:, :):: X
		double precision, allocatable, dimension(:)::y, alphas
		character(len=20)::kernelFunction
		double precision::b
		double precision, allocatable, dimension(:)::w 
	end type modelL
	type(modelL)::model
	

	p = 0
	pred = 0

	if(trim(KernelFunction).eq.'linearKernel')then
			
   		p = matmul(X, model%w) + model%b
		
	else if (trim(KernelFunction).eq.'gaussianKernel')then
		 XX = X * X
		 X1 = XX(:,1)+ XX(:,2) 
	  
		 XM = model%X * model%X 
		 X2 = XM(:, 1) + XM(:, 2) 
		 
		K = -2 * matmul(X, transpose(model%X))  !K(M, idx)
	
		 do i=1,M
		 K(i, :) = X2(:) + K(i, :)  
		 enddo
		 do i=1,idx
		 K(:, i) = X1(:) + K(:, i) 
		 enddo

		 x11 = (/1/)
		 x12 = (/0/)
		 sigma=0.1
		 M1 =1
    	 K = gaussiankernel(M1, x11, x12, sigma) ** K
		

		do i=1, M
			K(i,:) = model%y(:) * K(i, :)
		enddo
		do i=1, M
			K(i,:) = model%alphas(:) * K(i, :)
		enddo
		
		do i=1, M
			p(i) = sum(K(i,:))
		enddo
		  
	else

	write(*,*)'wrong!'
	stop
	end if

!% Convert predictions into 0 / 1
	pred = 0
	do i=1, M
	if(p(i) >= 0) then
		pred(i) = 1
	endif
	enddo

	end subroutine svmPredict

