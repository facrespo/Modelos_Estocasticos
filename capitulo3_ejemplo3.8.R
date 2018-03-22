library(lattice);
library(xtable);

K=20 #Etapas que se van a calcular.

str_eval=function(x) {return(eval(parse(text=x)))};

P =t(matrix(c(0.5,0,0.5,0,0,0,0,
              0.33,0,0,0.67,0,0,0,
              0,0,0.33,0,0.67,0,0,
              0.25,0.25,0,0.25,0,0.25,0,
              0.33,0,0.33,0,0.34,0,0,
              0,0,0,0,0,0.33,0.67,
              0,0,0,0,0,0.67,0.33), nrow=7, ncol=7));

r=nrow(P);

iterate.P <- function(x, P, n) {
  res <- matrix(NA, n+1, length(x))
  res[1,] <- x
  for (i in seq_len(n))
    res[i+1,] <- x <- x %*% P
  res
}


y1 <- iterate.P(c(1, 0, 0, 0, 0, 0, 0), P, K);
y2 <- iterate.P(c(0, 1, 0, 0, 0, 0, 0), P, K);
y3 <- iterate.P(c(0, 0, 1, 0, 0, 0, 0), P, K);
y4 <- iterate.P(c(0, 0, 0, 1, 0, 0, 0), P, K);
y5 <- iterate.P(c(0, 0, 0, 0, 1, 0, 0), P, K);
y6 <- iterate.P(c(0, 0, 0, 0, 0, 1, 0), P, K);
y7 <- iterate.P(c(0, 0, 0, 0, 0, 0, 1), P, K);

v <- eigen(t(P), FALSE)$vectors[,1]
v <- v/sum(v) # normalise eigenvector

matplot(x=(0:K), y1, type="l", lty=1, xlab="Etapas", ylab="Probabilidad");
matlines(0:K, y2, lty=2);
matlines(0:K, y3, lty=3);
matlines(0:K, y4, lty=4);
matlines(0:K, y5, lty=5);
matlines(0:K, y6, lty=6);
matlines(0:K, y7, lty=7);
legend(10, 1.05, legend=c("1","2","3","4","5","6","7"), col=1:7, lty=1:7);
#points(rep(20, 7), v, col=1:7)

run <- function(i, P, n) {
  res <- integer(n)
  for (t in seq_len(n))
    res[[t]] <- i <- sample(nrow(P), 1, pr=P[i,])
  res
};

samples <- run(1, P, 50);
plot(samples, type="s", xlab="Etapas", ylab="Estados", las=1);
