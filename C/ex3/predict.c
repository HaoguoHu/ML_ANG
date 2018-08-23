#include <stdio.h>
double sigmoid(double z);

int predict(int M, int N, int M1, int N1, int M2, int N2, double X[M][N], double Theta1[M1][N1],double Theta2[M2][N2], double g[M])
{

        double TX, ggg, max;
        int i, j, k;
	  static double gg[5000][26];
	  static double X1[5000][401];
  
	  
	  for(i=0; i < M; i++){
	       X1[i][0] = 1;
		 for(j=1; j < N1; j++)
		    X1[i][j] = X[i][j-1];
	   }
		 

        for(i=0; i < M; i++){	      
	       for(k=0; k < M1; k++){
	           TX = 0;
                 for(j=0; j < N1; j++)
                    TX += X1[i][j] * Theta1[k][j];
		     
                 gg[i][k+1] = sigmoid(TX);
		 }		 		
	           gg[i][0] = 1;
        }
		
		
		
	     for(i=0; i < M; i++){
	        max = -1;
		  
	       for(k=0; k < M2; k++){
	           TX = 0;
                 for(j=0; j < N2; j++)
                      TX += gg[i][j] * Theta2[k][j];
		     
                 ggg = sigmoid(TX);		     	   
		   
		     if(ggg >  max ){
			  max = ggg;
		        g[i] = k+1;
			  if(g[i] == 0)g[i]=10;      
		     }
	        }	     	     
           }	  

        return 0 ;
}
