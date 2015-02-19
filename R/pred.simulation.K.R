pred.simulation.K <- function (data, log.K) 
{
  betageom.bf.K = function(theta, s) {
    y = s$y
    I = s$I
    K = s$K
    eta = exp(theta)/(1 + exp(theta))
    N = length(y[I])
    logf = function(y, K, eta) lbeta(K * eta + I, K * (1 - eta) + y) - 
                lbeta(K * eta, K * (1 - eta))
    sum(logf(y, K, eta)) - lbeta(N, sum(y))
  }
  compute.log.bf.K = function(index) {
    find.gaps = function(x) {
      n = length(x)
      ab.hit = c((1:n)[x == 1], n + 1)
      y = diff(c(0, ab.hit)) - 1
      m = length(y)
      I = c(rep(1, m - 1), 0)
      if (y[m] == 0) {
        y = y[1:(m - 1)]
        I = I[1:(m - 1)]
      }
      list(y = y, I = I)
    }
    y = rbinom(n[index], 1, p[index])
    gaps = find.gaps(y)
    ifelse(sum(y) > 2, laplace(betageom.bf.K, 0, list(y = gaps$y, 
                            I = gaps$I, K = exp(log.K)))$int, 0)
  }

  h = data[, 1]
  n = data[, 2]
  N = length(n)
  fit = laplace(betabinexch, c(1, 1), cbind(h, n))
  eta = exp(fit$mode[1])/(1 + exp(fit$mode[1]))
  K = exp(fit$mode[2])
  p = (h + K * eta)/(n + K)
  sapply(1:N, compute.log.bf.K)
}