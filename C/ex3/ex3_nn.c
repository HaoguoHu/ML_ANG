#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int predict(int M, int N, int M1, int N1, int M2, int N2, double X[M][N], double Theta1[M1][N1],double Theta2[M2][N2], double g[M]);

int main()
{
int M = 5000, N = 400, M1 = 25, N1 = 401, M2 = 10, N2 = 26;    
FILE *fp;
char line[6017];  //401*15 + 2 /// 2='\n'
char line2[392];  //26*15 + 2 /// 2='\n'
int i, j, k, ri;	
static double X[5000][400] ;  //array is large then have to use "static"
static double y[5000] ;
double sel[100][N];
int index[100];
double lambda = 0.1;	
static double Theta1[25][401], Theta2[10][26];

if((fp = fopen ("ex3data1.txt", "r")) == NULL){
		printf("open wrong 1\n");
	}else{
	       i = 0;
	       while(fgets(line, sizeof line, fp) != NULL){
//		   printf("line[%d]=  %s \n", i, line); 
		   
		   X[i][0] = (double)atof(strtok (line, " "));
		   	
		   for(j=1; j < N; j++)	   	
    	         X[i][j] = (double)atof(strtok (NULL, " "));
		  
		   y[i] = (double)atoi(strtok (NULL, ","));			
		   
//		   printf("%.5f, %.5f ,%.0f\n", X[i][0], X[i][1],  y[i]);
//               getchar();

		   i++;	   
	       }
   fclose(fp);
}
	 
if((fp = fopen ("ex3weights.txt", "r")) == NULL){
		printf("open wrong 2\n");
	}else{

		 for(i=0; i < M1; i++){
	           fgets(line, sizeof line, fp);
//		     printf("line[%d]=  %s \n", i, line); 		   
		     Theta1[i][0] = (double)atof(strtok (line, " "));	   	
		     for(j=1; j < N1; j++)	   	
    	                Theta1[i][j] = (double)atof(strtok (NULL, " "));		  				   
		 }
			
		 for(i=0; i < M2; i++){	 
		      fgets(line2, sizeof line2, fp);
//			 printf("line2[%d]=  %s \n", i, line2); 
			 Theta2[i][0] = (double)atof(strtok (line2, " "));	 
		      for(j=1; j < N2; j++)	   	
    	                Theta2[i][j] = (double)atof(strtok (NULL, " "));		  				   
		 }		    	   
	
      fclose(fp);
}
	 
// Randomly select 100 data points to display
index[0] = 0;
for(i=0; i < 100; i++){
     ri = rand()%M;     
     for(j=0; j < i; j++){
     	  if(index[j] != ri){
	  	 index[i] = ri;
		 
	  	for(k=0; k < N; k++)
     	         sel[i][k] = X[ri][k];	     
	  }
     }
}
//displayData(sel);



//================ Part 3: Predict for One-Vs-All ================

	int ta =0;
	double g[M];
	
      printf("p= %d \n", predict(M, N, M1, N1, M2, N2, X, Theta1, Theta2, g));

	for(i=0; i < M; i++){
//		   printf("%d  %lf  %lf\n", i, g[i], y[i]);
	   if(fabs(y[i] - g[i]) <= 0.1)
	   	ta++;
	}
	printf("Train Accuracy: %d   %d  %f%\n", ta, M,  (double)ta/(double)M * 100);

 exit(0);

}
