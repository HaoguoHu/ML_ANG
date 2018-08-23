#include <math.h>

int debugInitializeWeights(int fan_out, int fan_in, double W[fan_out][fan_in])
{

	int i, j, k;
	double T[ fan_out *  fan_in];

	 
	for(i=0; i<fan_out; i++){
		for(j=0; j<fan_in; j++)
			W[i][j] = 0;
	}
  
		k = 0;
	for(i=0; i<fan_out; i++){
		for(j=0; j<fan_in; j++){
			T[k] = sin(W[i][j])/10.;
			k = k + 1;	
		}
	}		

			  k = 0;
	for(j=0; j<fan_in; j++){
		for(i=0; i<fan_out; i++){
			W[i][j] = T[k];	
			k = k + 1;	
		}
	}		

	return 0;
}
