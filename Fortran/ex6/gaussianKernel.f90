	function gaussianKernel(M1, x1, x2, sigma)
	implicit none
	
	integer:: M1, i
	double precision,dimension(M1)::x1, x2
	double precision::sigma, gaussianKernel

	gaussianKernel = exp(-1. * sum((x1-x2)*(x1-x2))/2./sigma**2)
	

	end
	
