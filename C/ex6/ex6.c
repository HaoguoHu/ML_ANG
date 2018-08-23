#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
	
int main()
{
	int M=51, N=2;
	double X[M][N], y[M];
	FILE *fp;
	char line[15*N+2];
	int i, j;
	
	printf("Loading and Visualizing Data ...\n");

	if((fp = fopen ("ex6data1.txt", "r")) == NULL){
		printf("open wrong \n");
	}else{
	       i = 0;
	       while(fgets(line, sizeof line, fp) != NULL){
//		   printf("line[%d]=  %s \n", i, line); 
			if(i < M){		   
		   		X[i][0] = (double)atof(strtok (line, " "));		    	
		   		X[i][1] = (double)atof(strtok (NULL, " "));
//		   printf("%15.10f, %15.10f \n", X[i][0],  X[i][1]);
//           getchar();
			}else{
				y[i-M] = (double)atof(strtok (line, " "));		    			 
			}					   

		   i++;	   
	       }
   	   fclose(fp);
	}
	
//	fp = popen ("gnuplot -persistent", "w");
//    fprintf(fp, "%s \n", "set title \"X y\"");
//	fprintf(fp, "%s \n", "plot 'ex6data1.txt' lt rgb \"violet\"  ");
//	pclose (fp);
	

    return 0;
	
	
	exit(0);
}
