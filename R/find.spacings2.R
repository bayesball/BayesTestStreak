find.spacings2 = function(x, k=1) {
  # will find all spacings until k successes
  # drop incomplete data at the end of sequence
  n <- length(x)
  ab.hit <- which(x == 1)
  succ.number <- rep(1:k, n)[1:length(ab.hit)]
  ab.k.hit <- ab.hit[succ.number == k]
  diff(c(0, ab.k.hit)) - k
}