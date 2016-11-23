library(lattice);

sigma=1;
largo=4;
s=as.matrix(seq(0, 9, by = 0.01));
nombre=character(largo);
dims=dim(s);
alpha= matrix(0, largo, 1);
r= matrix(0, dims[1], largo);
for (K in 1:largo) {
  alpha[K]=K;
  nombre[K]=as.character(alpha[K]);
  r[,K]=(1/sigma)*(dnorm(s, alpha[K], sigma, log = F)/(1-pnorm(s, alpha[K], sigma, lower.tail = T, log.p = F)));
}

plot(s,r[,1],type='l',lty=1,ylab='Tasa de falla');
lines(s,r[,2],type='l',lty=2);
lines(s,r[,3],type='l',lty=3);
lines(s,r[,4],type='l',lty=5);
legend(8, 8.5, nombre, cex=0.8, lty=1:4, title="media");

