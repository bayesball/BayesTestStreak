\name{permutation.test}
\alias{permutation.test}
\title{
Permutation Test of a Binary Sequence
}
\description{
Given a binary sequence of 0's and 1's, computes the p-value of a permutation test of randomness using a simulation-based permutation test.  The sum of squared spacings is used as the measure of streakiness (the clumpiness statistic).
}
\usage{
permutation.test(x, ITER = 1000)
}
\arguments{
  \item{x}{
vector of 0's and 1's
}
  \item{ITER}{
number of iterations of simulation
}
}
\value{
the p-value, the probability that the clumpiness statistic under the randomness model is at least as large as the observed value
}
\references{
Albert, J. (2013), Looking at Spacings to Access Streakiness, Journal of Quantitative Analysis of Sports, Vol 9, Issue 2, 151-163.
}
\author{
Jim Albert
}

\examples{
y <- rgeom(500, prob=0.3)
permutation.test(y)
}
