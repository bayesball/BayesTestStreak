betageom <- function(theta, s) {
  y <- s$y
  I <- s$I
  eta <- exp(theta[1]) / (1 + exp(theta[1]))
  K <- exp(theta[2])
  logf <- function(y, K, eta) 
    lbeta(K * eta + I, K * (1 - eta) + y) - 
    lbeta(K * eta, K * (1 - eta))
  sum(logf(y, K, eta)) + 
    theta[2] - 2 * log(1 + exp(theta[2]))
}