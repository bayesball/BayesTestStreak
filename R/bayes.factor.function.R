bayes.factor.function <- function(d, log.K=seq(1,7,by=.1), plot=FALSE)
{
# input: d - vector of 0's and 1's

#################################### functions
betageom.bf.K = function(theta, s) {
    y = s$y
    I = s$I
    K = s$K
    eta = exp(theta)/(1 + exp(theta))
    N = length(y[I])
    logf = function(y, K, eta) lbeta(K * eta + I, 
        K * (1 - eta) + y) - lbeta(K * eta, K * (1 - eta))
    sum(logf(y, K, eta)) - lbeta(N, sum(y))
  }
compute.log.bf.K=function(log.K,y)
{
find.gaps = function(x) {
    # revised to add last spacing
    # output is list with two components
    # y - vector of spacings
    # I - indicator vector, 0 if last spacing doesn't end with 1
    n = length(x)
    ab.hit = c((1:n)[x == 1], n + 1)
    y = diff(c(0, ab.hit)) - 1
    m = length(y)
    I = c(rep(1, m-1), 0)
    if(y[m] == 0){
      y=y[1:(m-1)]
      I=I[1:(m-1)]
    }
    list(y = y, I = I)
}
gaps=find.gaps(y)
laplace(betageom.bf.K,0,
        list(y=gaps$y, I=gaps$I, K=exp(log.K)))$int
}

################################################

log.BF=sapply(log.K, compute.log.bf.K, d)
max.log.BF=max(0, max(log.BF))
max.log.K=ifelse(max.log.BF>0, 
  log.K[log.BF==max.log.BF], NA)

if(plot==TRUE)
{
plot(log.K, log.BF, type="l", xlab="log K",
  ylab="log Bayes Factor", lwd=3, col="red",
  main=paste("max log BF =",round(max.log.BF,1)))
abline(h=0, col="blue", lwd=3)
abline(v=max.log.K, lty=2, lwd=3, col="green")
}

list(log.K=log.K, log.BF=log.BF, 
  max.log.BF=max.log.BF, max.log.K=max.log.K)
}
