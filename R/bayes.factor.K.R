bayes.factor.K = function(y, log.K) {
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
  gaps = find.gaps(y)
  laplace(betageom.bf.K, 0, 
          list(y = gaps$y, I = gaps$I, K = exp(log.K)))$int
}