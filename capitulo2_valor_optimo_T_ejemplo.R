library(lattice);

b2=15;
b1=10;
largo=1;
lambda=as.matrix(seq(0.1, 9, by = 0.1));
alpha=as.matrix(seq(1.1, 10, by = 0.1));
nombre=character(largo);
dims=dim(lambda);
T= matrix(0, dims[1],dims[1]);
for (i in 1:dims[1]) {
  for (j in 1:dims[1]){
      comodin=b2/(lambda[i]*(alpha[j]-1)*b1)
      T[i,j]=exp((1/alpha[j])*log(comodin));
  }
}

persp(lambda, alpha, T, expand = 0.6, theta = 30, phi=10);


