beta_negbinom <- function(theta, s) {
  eta <- exp(theta[1]) / (1 + exp(theta[1]))
  K <- exp(theta[2])
  logf <- function(y, k, K, eta) 
    lbeta(K * eta + k, K * (1 - eta) + y) - 
    lbeta(K * eta, K * (1 - eta))
  sum(logf(s$y, s$k, K, eta)) + 
    theta[2] - 2 * log(1 + exp(theta[2]))
}
