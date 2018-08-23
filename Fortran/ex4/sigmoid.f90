	function sigmoid(z)
	  implicit none
	  double precision :: z, sigmoid
	

        sigmoid = 1. / (1 + exp(-1 * z))
	
	
	end function sigmoid
