#include <stdio.h>
double sigmoid(double z);

int predictOneVsAll(int M, int N, double all_theta[10][N], double X[M][N], double g[M])
{
        double TX;
        int i, j, k;
	  double gg;
	  double max;

        for(i=0; i < M; i++){
	     g[i] = 0;
	     max = -1;
	     
	     for(k=0; k < 10; k++){
	         TX = 1;
              for(j=0; j < N; j++)
                 TX += X[i][j] * all_theta[k][j];
		     
               gg = sigmoid(TX);
		   
		   if(gg >  max ){
			max = gg;
		      g[i] = k;
			if(g[i] == 0)g[i]=10;      
		  }
	     }
	     	     
        }	  

        return 0 ;
}
