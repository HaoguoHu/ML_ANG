	function linearKernel(M, x1, x2)
	implicit none
	
	integer::M, i
	double precision, dimension(M)::x1, x2
	double precision:: linearKernel
	
		linearKernel = 0
	do i=1, M
		linearKernel = linearKernel + x1(i) * x2(i)
	enddo
	
	end function linearKernel
