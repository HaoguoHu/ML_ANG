int LMS(double X[],double Y[],int M, double theta[])
{	
	double  a1=0, b1=0, c1=0;
	double  a2=0, b2=0, c2=0;
	int i;
		
		a1 = M;
	for(i=0; i < M; i++){
		b1 = b1 + X[i];
		c1 = c1 + Y[i];
		b2 = b2 + X[i]*X[i];
		c2 = c2 + X[i]*Y[i];
	}
		a2 = b1;	
	theta[1] = (a2*c1-a1*c2)/(a2*b1-a1*b2);
	theta[0] = (b2*c1-b1*c2)/(a1*b2-a2*b1);
	
	return 0;	
}

int transpose(int M, int N, double X[M][N], double XT[N][M]) //!transpose matrix from  X(M,N) to matrix XT(N,M)
{
	int i, j;	
	
	for(i=0; i < N; i++){
	for(j=0; j < M; j++){
	   XT[i][j] = X[j][i];
	}
	}
	
	return 0;
}
	
int multiply(int M, int N, int L, double X[M][N], double Y[N][L], double XML[M][L]) //!multiply matrix  X(M,N) to matrix Y(N,L)
{
	int i, j, k;	
	
	for(i=0; i < M; i++){
	for(j=0; j < L; j++){ 
	   XML[i][j]=0.;
	   for(k=0; k < N; k++){
	     XML[i][j] = XML[i][j] + X[i][k] * Y[k][j];
	   }
	}
	}
	
	return 0;
}
	
		
	
	
