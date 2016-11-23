library(lattice);


lambda=1;
largo=5;
s=as.matrix(seq(0, 9, by = 0.01));
nombre=character(largo);
dims=dim(s);
alpha= matrix(0, largo, 1);
r= matrix(0, dims[1], largo);
for (K in 1:largo) {
  if(K==5){
    alpha[K]=4;
  } else {
  alpha[K]=(1/2)*K;
  }
  nombre[K]=as.character(alpha[K]);
  r[,K]=dgamma(s, alpha[K], lambda, log = F)/(1-pgamma(s, alpha[K], lambda, lower.tail = T, log.p = F));
}

plot(s,r[,1],type='l',lty=1,ylab='Tasa de falla',ylim=c(0,2));
lines(s,r[,2],type='l',lty=2);
lines(s,r[,3],type='l',lty=3);
lines(s,r[,4],type='l',lty=4);
lines(s,r[,5],type='l',lty=5);
legend(7.5, 2, nombre, cex=0.8, lty=1:5, title="alpha");

