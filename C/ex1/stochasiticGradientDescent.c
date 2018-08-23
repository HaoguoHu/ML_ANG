int stochasiticGradientDescent(int M, double alpha,  double X[], double Y[], double theta[])
{
	int i;
	double temp0, temp1;

		theta[0]  = 0;
    		theta[1]  = 0;
    
    	for(i=0; i < M; i++){
        	theta[0] = theta[0] - alpha*(theta[0] + theta[1] * X[i] - Y[i]) * 1;
        	theta[1] = theta[1] - alpha*(theta[0] + theta[1] * X[i] - Y[i]) * X[i];
    	}
		
	
	return 0;
}
		
