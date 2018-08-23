!double precision function fmincg(length, nn_params, input_layer_size, hidden_layer_size, num_labels, inputdata, y, lambda)
   
!	subroutine fmincg(M, N, X, y, nn_params)
	subroutine fmincg(M,N,M1,N1,M2,N2, nn_params,X, y,lambda, length)
       
	use ieee_arithmetic
	implicit none
	 
	integer::M, N, M1,N1,M2,N2
	double precision,dimension(M,N)::X		 
	double precision,dimension(M,1)::y		  	 
    
    ! Copyright (C) 2001 and 2002 by Carl Edward Rasmussen. Date 2002-02-13
    ! (C) Copyright 1999, 2000 & 2001, Carl Edward Rasmussen
    ! 
    ! Permission is granted for anyone to copy, use, or modify these
    ! programs and accompanying documents for purposes of research or
    ! education, provided this copyright notice is retained, and note is
    ! made of any changes that have been made.
    ! 
    ! These programs and documents are distributed without any warranty,
    ! express or implied.  As the programs were written for research
    ! purposes only, they have not been tested to the degree that would be
    ! advisable in any important application.  All use of these programs is
    ! entirely at the user's own risk.
    !
    ! [ml-class] Changes Made:
    ! 1) Function name and argument specifications
    ! 2) Output display
    !
    ! [John Shahbazian http://fortrandev.wordpress.com 5/31/2014] Changes Made:
    ! 1) Ported to Fortran
    ! 2) Changed the cost function call to internal.  Replace the
    !    'costandgradient' function to whatever you would like.  It returns
    !    the cost as the result, and the gradient is returned through the first 
    !    argument as intent(inout) (e.g. 'gradient#').  
    ! 3) Changed the variable names to be readable.
    !    f1 = cost1
    !    df1 = grad1
    !    s = search_direction
    !    d1 = slope1
    !    z1 = point1
    !    X0 = backup_params
    !    f0 = cost_backup
    !    df0 = gradient_backup    
    
!    integer, intent(in) :: length,input_layer_size,hidden_layer_size ,num_labels
!    integer :: length
!      double precision, allocatable, intent(in) :: inputdata(:,:), y(:,:)
!      double precision, allocatable  :: inputdata(:,:)
!    double precision, intent(in) :: lambda
	double precision:: lambda
    double precision:: nn_param(M1*N1+M2*N2)
	double precision:: nn_params(M1*N1+M2*N2, 1)

    
    double precision :: RHO = 0.01                            ! a bunch of constants for line searches
    double precision :: SIG = 0.5       ! RHO and SIG are the constants in the Wolfe-Powell conditions
    double precision :: INT = 0.1    ! don't reevaluate within 0.1 of the limit of the current bracket
    double precision :: EXT = 3.0                    ! extrapolate maximum 3 times the current bracket
    integer :: MAXEVALS = 20                         ! max 20 function evaluations per line search
    double precision :: RATIO = 100                                      ! maximum allowed slope ratio

    double precision :: mintemp, minstuff, MM, A, B
    double precision :: fX = 0.0

    integer :: success
    integer :: costFunctionCount  ,length                                       ! zero the run length counter
    integer :: ls_failed = 0                                    ! no previous line search has failed

    double precision, allocatable :: backup_params(:,:)
    double precision :: cost1, cost2, cost3, cost_backup
    double precision, allocatable :: grad1(:,:), grad2(:,:), gradient3(:,:), gradient_backup(:,:), tmp(:,:)
    double precision :: limit
    double precision :: point1, point2, point3, ptemp
    double precision, allocatable :: search_direction(:,:), stemp(:,:)
    double precision :: slope1, slope2, slope3, slope_vector(1,1)
    double precision :: sqrtnumber
    double precision :: sd_calc_1(1,1), sd_calc_2(1,1), sd_calc_3(1,1), sd_calc_4
    double precision, allocatable :: sd_calc_5(:,:)


    allocate(grad1(1:size(nn_params),1:1))
    allocate(grad2(1:size(nn_params),1:1))
    allocate(gradient3(1:size(nn_params),1:1))
    allocate(gradient_backup(1:size(nn_params),1:1))
    allocate(tmp(1:size(nn_params),1:1))
    allocate(search_direction(1:size(nn_params),1:1))
    allocate(stemp(1:size(nn_params),1:1))
    allocate(backup_params(1:size(nn_params),1:1))
	allocate(sd_calc_5(size(search_direction,1),size(search_direction,2)))
	

!	 length = 50
	 costFunctionCount=0  !Very important
	 

!    cost1 = costandgradient(grad1,nn_params,input_layer_size,hidden_layer_size,num_labels, inputdata, y, lambda)

	  call nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, cost1, grad1)
!      if(length < 0)then
!        costFunctionCount = costFunctionCount + 1    !(length<0)                                          ! count epochs?!
!      endif
	
    search_direction = -grad1                               ! search direction is steepest
    slope_vector = matmul(-transpose(search_direction),search_direction)    ! this is the slope
    slope1 = slope_vector(1,1)                                  !convert to scalar
    point1  = 1.0/(1.0-slope1)                                  ! initial step is red/(|s|+1)
	
    do while(costFunctionCount < abs(length)) 
                                            
!        costFunctionCount = costFunctionCount + (length>0)                                      ! count iterations?!
	 if(length > 0)then
	    costFunctionCount = costFunctionCount+1
	 endif
        backup_params = nn_params
        cost_backup = cost1
        gradient_backup = grad1                             ! make a copy of current values
        stemp = point1  * search_direction
        nn_params = nn_params + stemp                           ! begin line search
	  
        grad2 = grad1
	    
!        cost2 = costandgradient(grad2,nn_params,input_layer_size,hidden_layer_size,num_labels, inputdata, y, lambda)
	 call nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, cost2, grad2)
	 !Pass too many parameters to subroutine will slow down the computing

        if(length < 0)then
          costFunctionCount = costFunctionCount + 1    !(length<0)                                          ! count epochs?!
        endif
!        costFunctionCount = costFunctionCount + (length<0)                                      ! count epochs?!

        slope_vector = matmul(transpose(grad2),search_direction)              ! this is the slope
        slope2 = slope_vector(1,1)                              !convert to scalar

        cost3 = cost1
        slope3 = slope1
        point3 = -point1                                        ! initialize point 3 equal to point 1
        if(length > 0)then
            MM = MAXEVALS
        else
            MM = min(MAXEVALS, -length-costFunctionCount)
        end if
        success = 0
        limit = -1                                              ! initialize quantities
	  
        do
            do while (( (cost2 > (cost1 + (point1 * RHO * slope1))) .or. (slope2 > (-SIG * slope1)) ) .and. (MM > 0) )
                limit = point1                                  ! tighten the bracket
                if(cost2 > cost1)then
                    point2 = point3 - (0.5 * slope3 * point3 * point3)/(slope3 * point3 + cost2 - cost3)                 ! quadratic fit
                else
                    A = 6*(cost2 - cost3)/point3 + 3*(slope2 + slope3)                                 ! cubic fit
                    B = 3*(cost3 - cost2) - point3 * (slope3 + 2*slope2)
                    point2 = (sqrt(B*B - A*slope2*point3*point3) - B)/A
                end if
                if(ieee_is_nan(point2) .or. (.not. ieee_is_finite(point2)))then
                    point2 = point3 / 2                         ! if we had a numerical problem then bisect
                end if
                point2 = MAX( MIN(point2, (INT * point3)), ((1.0 - INT) * point3))  ! don't accept too close to limits
                point1  = point1 + point2                       ! update the step
                stemp = point2 * search_direction
                nn_params = nn_params + stemp

!                cost2 = costandgradient(grad2,nn_params,input_layer_size,hidden_layer_size,num_labels, inputdata, y, lambda)
		   call nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, cost2, grad2)
		   
                MM = MM - 1
!                costFunctionCount = costFunctionCount + (length<0)                              ! count epochs?!
                  if(length < 0)then
        			costFunctionCount = costFunctionCount + 1    !(length<0)                                          ! count epochs?!
      		endif

                slope_vector = matmul(transpose(grad2),search_direction)
                slope2 = slope_vector(1,1)                      !convert to scalar
                point3 = point3 - point2                        ! point3 is now relative to the location of point2
            end do
	 
!	      write(*,*)'cost2=', cost2, 'cost1=', cost1
		
            if ((cost2 > (cost1 + (point1*RHO*slope1)) ) .or. (slope2 > (-SIG * slope1) ))then
                exit                                            ! this is a failure
            elseif (slope2 > (SIG * slope1))then
                success = 1
                exit                                            ! success
            elseif (MM == 0)then
                exit                                            ! failure
            end if
            A = 6*(cost2 - cost3)/point3 + 3*(slope2 + slope3)  ! make cubic extrapolation
            B = 3*(cost3 - cost2) - point3*(slope3 + 2*slope2)
            sqrtnumber = (B*B) - A*slope2*point3*point3
            if((.not. ieee_is_normal(sqrtnumber)) .or. sqrtnumber < 0 )then
                if (limit < -0.5) then                          ! if we have no upper limit
                    point2 = point1  * (EXT - 1)                ! the extrapolate the maximum amount
                else
                    point2 = (limit - point1)/2                 ! otherwise bisect
                end if
            else
                point2 = (-slope2 * point3 * point3)/(B + sqrt(sqrtnumber))
                if ((limit > -0.5) .and. ((point2 + point1) > limit))then          ! extraplation beyond max?
                    point2 = (limit - point1)/2                 ! bisect
                elseif ((limit < -0.5) .and. ((point2 + point1) > (point1 * EXT)))then       ! extrapolation beyond limit
                    point2 = point1 * (EXT - 1.0)               ! set to extrapolation limit
                elseif (point2 < (-point3 * INT))then
                    point2 = -point3 * INT
                elseif ((limit > -0.5) .and. (point2 < (limit - point1)*(1.0 - INT)))then   ! too close to limit?
                    point2 = (limit - point1 ) * (1.0 - INT)
                end if
            end if

            cost3 = cost2
            slope3 = slope2
            point3 = -point2               
            point1  = point1  + point2

            stemp = point2 * search_direction
            nn_params = nn_params + stemp                       ! update current estimates

 !           cost2 = costandgradient(grad2,nn_params,input_layer_size,hidden_layer_size,num_labels, inputdata, y, lambda)
		call nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, cost2, grad2)
	 
            MM = MM - 1
!            costFunctionCount = costFunctionCount + (length<0)                                  ! count epochs?!
		if(length < 0)then
        		costFunctionCount = costFunctionCount + 1    !(length<0)                                          ! count epochs?!
      	endif
            slope_vector = matmul(transpose(grad2),search_direction)
            slope2 = slope_vector(1,1)                          !convert to scalar
        end do                                                  ! end of line search

		
        if (success == 1)then                                   ! if line search succeeded
            cost1 = cost2
            fX = cost1
			write(*,*)'sucess Cost =', cost1

            ! this calculation was split up to make it easier to work with
            sd_calc_1 = matmul(transpose(grad2),grad2)
            sd_calc_2 = matmul(transpose(grad1),grad2)
            sd_calc_3 = matmul(transpose(grad1),grad1)
            sd_calc_4 = (sd_calc_1(1,1) - sd_calc_2(1,1)) / sd_calc_3(1,1)
            sd_calc_5 = sd_calc_4 * search_direction
            search_direction = sd_calc_5 - grad2

            tmp = grad1
            grad1 = grad2
            grad2 = tmp                                     ! swap derivatives
            slope_vector = matmul(transpose(grad1),search_direction)
            slope2 = slope_vector(1,1)                          !convert to scalar
            if(slope2 > 0)then                                  ! new slope must be negative
                search_direction = -grad1                   ! otherwise use steepest direction
                slope_vector = matmul(-transpose(search_direction),search_direction)
                slope2 = slope_vector(1,1)                      !convert to scalar
            end if
            mintemp = slope1/(slope2 - 2.2251D-308)  !2.2551D-308 is min value double precision float
            minstuff = min(RATIO, mintemp)
            point1  = point1  * minstuff                         ! slope ratio but max RATIO
            slope1 = slope2
            ls_failed = 0                                        ! this line search did not fail
        else
            nn_params = backup_params
            cost1 = cost_backup
            grad1 = gradient_backup                         ! restore point from before failed line search
            if (ls_failed == 1 .or. costFunctionCount > abs(length))then        ! line search failed twice in a row
                exit                                            ! or we ran out of time, so we give up
            end if
            tmp = grad1
            grad1 = grad2
            grad2 = tmp                                    ! swap derivatives
            search_direction = -grad1                      ! try steepest
            slope_vector = matmul(-transpose(search_direction),search_direction)
            slope1 = slope_vector(1,1)                         ! convert to scalar
            point1  = 1.0 / (1.0 - slope1)
            ls_failed = 1                                      ! this line search failed
        end if

    end do


!    fmincg = fX  
!end function fmincg
end subroutine fmincg
