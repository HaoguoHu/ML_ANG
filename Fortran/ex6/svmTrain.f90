	subroutine  svmTrain(M, N, X, y0, C, kernelFunction, tol, max_passes, model, idx)
	implicit none	

	integer,parameter::M1=1
	integer::M, N
	double precision,dimension(M,N)::X, XX
	double precision,dimension(N,M)::XT
	double precision,dimension(M,M)::K
	double precision,dimension(M)::y, y0, alphas, E, X2
	double precision,dimension(M1)::x11, x12

	double precision:: b, b1, b2, eta, L, H, C, tol, alpha_i_old, alpha_j_old
	double precision::random, random1, gaussianKernel, sigma
	
	character(len=20)::kernelFunction
	
	type::modelL
		double precision, allocatable, dimension(:, :)::X
		double precision, allocatable, dimension(:)::y, alphas
		character(len=12)::kernelFunction
		double precision::b
		double precision, allocatable, dimension(:)::w
	end type modelL
	type(modelL)::model
	
	integer:: i, max_passes, dots, j, num_changed_alphas, passes, idx						 
 
		y = y0
!	do i=1, M
!		if(y0(i) == 0 ) y(i) = -1.
!	enddo
	
	where(y0 == 0) y = -1
			
	alphas = 0
	E = 0
	b = 0
	passes = 0
	eta = 0
	L = 0
	H = 0
	
	K = 0
	XT = 0

	
	if(trim(KernelFunction).eq.'linearKernel')then
	  write(*,*)'kernel function linear !'	
	  XT = transpose(X)	
	  K = matmul(X, XT)
	  write(*,*)shape(K)
	else if (trim(KernelFunction).eq.'gaussianKernel')then
		 XX = X * X
		 X2 = XX(:,1)+ XX(:,2)  
		 
		 K = -2 * matmul(X, transpose(X))
		 do i=1,M
		 K(i, :) = X2(:) + K(i, :)  
		 enddo
		 do i=1,M 
		 K(:, i) = X2(:) + K(:, i) 
		 enddo
!		 write(*,'(a,5f10.6)')'ok 57', (K(1,i),i=1,5)
		 
		 
		 x11 = (/1/)
		 x12 = (/0/)
		 sigma=0.1
    	 K = gaussiankernel(M1, x11, x12, sigma) ** K		 
		 write(*,*)		
		 write(*,'(a,6f10.6)')	'ok 55', (K(1,j),j=809,813), gaussiankernel(m1, x11, x12, sigma) 
		 write(*,*)'gaussianKernel',shape(K)
		
	else
		write(*,*)'wrong !, stopped at svmTrain.f90'
		stop
!		 K = zeros(m);
!    	for i = 1:m
!        for j = i:m
!             K(i,j) = kernelFunction(X(i,:)', X(j,:)');
!             K(j,i) = K(i,j); %the matrix is symmetric
!        end
	endif
	
!	 write(*,*)size(X), size(XT)
!	 write(*,*)shape(X), shape(XT)

!if strcmp(func2str(kernelFunction), 'linearKernel')
!    % Vectorized computation for the Linear Kernel
!    % This is equivalent to computing the kernel on every pair of examples
!    K = X*X';
!elseif strfind(func2str(kernelFunction), 'gaussianKernel')
!    % Vectorized RBF Kernel
!    % This is equivalent to computing the kernel on every pair of examples
!    X2 = sum(X.^2, 2);
!%	fprintf('X2= %f \n', X2);
!%	pause;
!    K = bsxfun(@plus, X2, bsxfun(@plus, X2', - 2 * (X * X')));
!    K = kernelFunction(1, 0) .^ K;
!else
!    % Pre-compute the Kernel Matrix
!    % The following can be slow due to the lack of vectorization
!    K = zeros(m);
!    for i = 1:m
!        for j = i:m
!             K(i,j) = kernelFunction(X(i,:)', X(j,:)');
!             K(j,i) = K(i,j); %the matrix is symmetric
!        end
!    end
!end


	write(*,*)'Training ...', max_passes
do while(passes <  max_passes )           
    num_changed_alphas = 0
    do i=1, M    
		E(i) = b + sum( alphas * y * K(:, i) ) - y(i)
			
        if ((Y(i)*E(i) < -tol .and. alphas(i) < C) .or. (Y(i)*E(i) > tol .and. alphas(i) > 0)) then
            
            ! In practice, there are many heuristics one can use to select
            ! the i and j. In this simplified code, we select them randomly.
			call random_number(random)				
				j = ceiling(M * random )
				
            do while( j == i) ! Make sure i \neq j
			    call random_number(random)
                j = ceiling(m * random) 
            enddo			

			E(j) = b + sum(alphas * y * K(:, j)) - y(j)			

            ! Save old alphas
            alpha_i_old = alphas(i)
            alpha_j_old = alphas(j)
            
            ! Compute L and H by (10) or (11). 
            if (Y(i) == Y(j))then
                L = max(0., alphas(j) + alphas(i) - C)
                H = min(C, alphas(j) + alphas(i))
            else
                L = max(0., alphas(j) - alphas(i))
                H = min(C, C + alphas(j) - alphas(i))
            endif
           
            if (L == H)then
                cycle
            endif

            ! Compute eta by (14).
            eta = 2 * K(i,j) - K(i,i) - K(j,j)
            if (eta >= 0)then
                ! cycle to next i. 
                cycle
            endif
            
            ! Compute and clip new value for alpha j using (12) and (15).
            alphas(j) = alphas(j) - (Y(j) * (E(i) - E(j))) / eta
            
            ! Clip
            alphas(j) = min (H, alphas(j))
            alphas(j) = max (L, alphas(j))
            
            ! Check if change in alpha is significant
            if (abs(alphas(j) - alpha_j_old) < tol)then
                ! cycle to next i. 
                ! replace anyway
                alphas(j) = alpha_j_old
                cycle
            endif
            
            ! Determine value for alpha i using (16). 
            alphas(i) = alphas(i) + Y(i)*Y(j)*(alpha_j_old - alphas(j))
            
            ! Compute b1 and b2 using (17) and (18) respectively. 
            b1 = b - E(i)  &
                 - Y(i) * (alphas(i) - alpha_i_old) *  K(i,j)  &
                 - Y(j) * (alphas(j) - alpha_j_old) *  K(i,j) 
            b2 = b - E(j) &
                 - Y(i) * (alphas(i) - alpha_i_old) *  K(i,j)  &
                 - Y(j) * (alphas(j) - alpha_j_old) *  K(j,j) 

            ! Compute b by (19). 
            if ((0 < alphas(i)) .and. (alphas(i) < C))then
                b = b1
            elseif ((0 < alphas(j)) .and. (alphas(j) < C))then
                b = b2
            else
                b = (b1+b2)/2
            endif					

            num_changed_alphas = num_changed_alphas + 1

        endif
        
    enddo  !do i=1, M 
    
    if (num_changed_alphas == 0)then
        passes = passes + 1
    else
        passes = 0
    endif

enddo  !do while ( max_passes  !while passes < max_passes,
write(*,*)' Done! '

! Save the model
	idx =0
	do i=1, M
		if(alphas(i) > 0 )idx = idx +1	
	enddo
	
	write(*,*)'idx=', idx
	allocate(model%X(idx,N))
	allocate(model%y(idx))
	allocate(model%alphas(idx))
	allocate(model%w(N))
	
	j=0
	do i=1, M
		if(alphas(i) > 0)then
			j = j + 1
			model%X(j,:)=X(i,:)
			model%y(j) = y(i)	
			model%alphas(j)= alphas(i)	
		endif
	enddo	
	
	model%kernelFunction = kernelFunction
	model%b= b

	do j=1, N
		model%w(j) = dot_product(alphas*y, X(:,j))
	enddo
	
!		write(*,*)model%b1, model%w1
		
!	deallocate(model%X1)
!	deallocate(model%y1)
!	deallocate(model%alphas1)
!!	deallocate(model%w1)

end subroutine svmTrain
