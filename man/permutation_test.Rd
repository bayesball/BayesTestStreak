\name{permutation_test}
\alias{permutation_test}
\title{
Permutation Test of a Binary Sequence
}
\description{
Given a binary sequence of 0's and 1's, computes the p-value of a permutation test of randomness using a simulation-based permutation test.  The sum of squared spacings is used as the measure of streakiness (the clumpiness statistic).
}
\usage{
permutation_test(x, ITER = 1000)
}
\arguments{
\item{streak_data}{
    data frame with components N and Outcome
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
streak_data <- data.frame(N = 1:500,
Outcome = rgeom(500, prob=0.3))
permutation_test(streak_data)
}
