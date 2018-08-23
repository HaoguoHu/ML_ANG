	function sigmoidGradient(z)
	  implicit none
	  double precision :: z, sigmoid, sigmoidGradient
	

        sigmoidGradient = sigmoid(z) * (1-sigmoid(z))
	
	
	end function sigmoidGradient
