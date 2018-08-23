	Module  mod_common
	implicit none
	
	integer,parameter::M=5000, N=400, num_labels=10
	double precision,dimension(M,N)::X
	double precision,dimension(M)::yy

	
	double precision::lambda=0.1
	
	save
	
	end Module mod_common
	
