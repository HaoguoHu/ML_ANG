int unroll(int M1,int N1,int M2,int N2,double theta1[M1][N1],double theta2[M2][N2],double nn_params[M1*N1+M2*N2])
{

	int i, j, k;

	k = 0;
	for(i=0; i<M1; i++){
		for(j=0; j<N1; j++){
			nn_params[k] = theta1[i][j];
			k = k + 1;
		}
	}

	for(i=0; i<M2; i++){
		for(j=0; j<N2; j++){
			nn_params[k] = theta2[i][j];
			k = k + 1;
		}
	}	
	
	return 0;
}

int reshape(int M1,int N1,int M2,int N2,double theta1[M1][N1],double theta2[M2][N2],double nn_params[M1*N1+M2*N2])
{

	int i, j, k;

	k = 0;
	for(i=0; i<M1; i++){
		for(j=0; j<N1; j++){
			 theta1[i][j] = nn_params[k];
	         k = k +1;
		}
	}

	for(i=0; i<M2; i++){
		for(j=0; j<N2; j++){
			theta2[i][j] = nn_params[k]; 
			k = k + 1;		
		}
	}
	

	return 0;
}

