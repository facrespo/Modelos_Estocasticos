library(lattice);
library(xtable);

str_eval=function(x) {return(eval(parse(text=x)))};

P =t(matrix(c(0.5,0.5,0,0,0.25,0.25,0.5,0,1/6,1/6,1/6,0.5,0,0,0,1), nrow=4, ncol=4));

test.svd=svd(P); 
test.svd;
summary(test.svd);
test.svd$d^2;# varianza.
r=nrow(P);
plot(test.svd$d, xlim = c(1, r), ylim=c(0,max(test.svd$d)), type = "b", pch = 16, xlab = "N�mero de vectores propios",ylab = "Valor vectores propios")
ve=test.svd$d^2/sum(test.svd$d^2);
plot(ve, xlim = c(1, r), ylim=c(0,max(ve)), type = "b", pch = 16, xlab = "N�mero de vectores propios",ylab = "varianza explicada")
for(i in 1:r){
  expr=parse(text=paste("B",as.character(i),"=","test.svd$u[,",as.character(i),"]%*%t(test.svd$v[,",as.character(i),"])",sep=""));
  eval(expr);
  expr=parse(text=paste("print(B",as.character(i),",include.rownames = FALSE, include.colnames = FALSE)",sep=""));
  eval(expr);
}


P2 =t(matrix(c(0.8,0.2,0.3,0.7), nrow=2, ncol=2));
test2.svd=svd(P2); 
test2.svd;
summary(test2.svd);
test2.svd$d^2;# varianza.
r2=nrow(P2);
plot(test2.svd$d, xlim = c(1, r2), ylim=c(0,max(test2.svd$d)), type = "b", pch = 16, xlab = "N�mero de vectores propios",ylab = "Valor vectores propios")
ve2=test2.svd$d^2/sum(test2.svd$d^2);
plot(ve2, xlim = c(1, r2), ylim=c(0,max(ve2)), type = "b", pch = 16, xlab = "N�mero de vectores propios",ylab = "varianza explicada")
for(i in 1:r2){
  expr=parse(text=paste("B2",as.character(i),"=","test2.svd$u[,",as.character(i),"]%*%t(test2.svd$v[,",as.character(i),"])",sep=""));
  eval(expr);
  expr=parse(text=paste("print(B2",as.character(i),",include.rownames = FALSE, include.colnames = FALSE)",sep=""));
  eval(expr);
}

run <- function(i, P, n) {
  res <- integer(n)
  for (t in seq_len(n))
    res[[t]] <- i <- sample(nrow(P), 1, pr=P[i,])
  res
};

samples <- run(1, P, 30);
samples1 <- run(1, P, 30);
samples2 <- run(1, P, 30);
samples3 <- run(1, P, 30);
plot(samples, type="s", xlab="Etapas", ylab="Estados", las=1);
lines(samples1,type="s",lty=2,lwd=1);
lines(samples2,type="s",lty=2,lwd=2);
lines(samples3,type="s",lty=3,lwd=1);
legend(22, 2, c("simulaci�n 1","simulaci�n 2","simulaci�n 3","simulaci�n 4"), cex=0.8, lty=c(1,2,2,3));
