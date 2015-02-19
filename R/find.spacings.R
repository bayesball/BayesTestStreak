find.spacings = function(x) {
  # revised to add last spacing
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