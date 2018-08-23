#include <stdio.h>
#include <stdlib.h>

int randInitializeWeights(int L_in, int L_out, double W[L_out][ L_in])
{
	double epsilon_init = 0.12;
	double ri;
	int i, j;
		
//	 srand(1);
	for(i=0; i<L_out; i++){
		for(j=0; j <= L_in; j++){
			ri = (double)(rand()%L_in)/(double)L_in; 		
			W[i][j] = ri * 2. * epsilon_init - epsilon_init;
		
//		printf("ri = %lf, %lf \n", ri, W[i][j]);
//		getchar();
	    }
	 }
}
