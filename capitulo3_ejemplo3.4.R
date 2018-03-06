library(lattice);
library(xtable);

K=20 #Etapas que se van a calcular.

str_eval=function(x) {return(eval(parse(text=x)))};

P =t(matrix(c(0.33,0.67,0,0,0,0.5,0.5,0,0,0.25,0,0.75,0,0,0.5,0.5), nrow=4, ncol=4));

r=nrow(P);

for (k in 1:K){
   if(k==1){
     expr=parse(text=paste("F",as.character(k),"=P",sep=""));
     eval(expr);
     print(k);
     print((F1),include.rownames = FALSE, include.colnames = FALSE);
   }
   else{
     Q=matrix(0,r,r);
     F=matrix(0,r,r);
     for(i in 1:r){
       Q=P;
       Q[,i]=0;
       expr=parse(text=paste("F[,i]=Q%*%as.matrix(F",as.character(k-1),"[,i])",sep=""));
       eval(expr);
     }
     print(k);
     expr=parse(text=paste("F",as.character(k),"=F",sep=""));
     eval(expr);
     expr=parse(text=paste("print((F",as.character(k),"),include.rownames = FALSE, include.colnames = FALSE)",sep=""));
     eval(expr);
   }
  }

#iterate.P <- function(x, P, n) {
#  res <- matrix(NA, n+1, length(x))
#  res[1,] <- x
#  for (i in seq_len(n))
#    res[i+1,] <- x <- x %*% P
#  res
#}


#y1 <- iterate.P(c(1, 0, 0, 0), P, K);
#y2 <- iterate.P(c(0, 1, 0, 0), P, K);
#y3 <- iterate.P(c(0, 0, 1, 0), P, K);
#y4 <- iterate.P(c(0, 0, 0, 1), P, K);


#matplot(0:K, y1, type="l", lty=1, xlab="Etapas", ylab="Probabilidad");
#matlines(0:K, y2, lty=2);
#matlines(0:K, y3, lty=3);
#matlines(0:K, y4, lty=4);
#legend(10, 1, c("simulación 1","simulación 2","simulación 3","simulación 4"), cex=0.8, col=plot_colors, lty=1:4);

run <- function(i, P, n) {
  res <- integer(n)
  for (t in seq_len(n))
    res[[t]] <- i <- sample(nrow(P), 1, pr=P[i,])
  res
};

samples <- run(1, P, 50);
plot(samples, type="s", xlab="Etapas", ylab="Estados", las=1);
