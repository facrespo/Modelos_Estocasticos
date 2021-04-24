library(lattice);

lambda=1;
largo=4;
s=as.matrix(seq(0, 9, by = 0.01));
nombre=character(largo);
dims=dim(s);
alpha= matrix(0, largo, 1);
r= matrix(0, dims[1], largo);
for (K in 1:largo) {
  alpha[K]=(1/2)*K;
  nombre[K]=as.character(alpha[K]);
  r[,K]=(lambda*alpha[K])*((s[])^{alpha[K]-1})
}

plot(s,r[,1],type='l',lty=1,ylab='Tasa de falla',cex.lab=1.5,cex.main=2.0);
lines(s,r[,2],type='l',lty=2);
lines(s,r[,3],type='l',lty=3);
lines(s,r[,4],type='l',lty=5);
legend(7.5, 4, nombre, cex=0.8, lty=1:4, title="alpha");

