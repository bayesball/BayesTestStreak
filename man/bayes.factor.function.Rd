\name{bayes.factor.function}
\alias{bayes.factor.function}
\title{
Computes Bayes Factors for Range of Alternatives
}
\description{
Computes the Bayes factor in support of a streaky model over a consistent model for a range of parameter values for the streaky model.
}
\usage{
bayes.factor.function(d, log.K = seq(1, 7, by = 0.1), plot = FALSE)
}
\arguments{
  \item{d}{
vector of binary outcomes (0's and 1's)
}
  \item{log.K}{
range of values of log(K) under the streaky model
}
  \item{plot}{
if TRUE, will construct a plot of the log Bayes factor
}
}

\value{
a list with components
  \item{log.K}{vector of values of log(K)}
  \item{log.BF}{vector of values of log(Bayes Factor)}
  \item{max.log.BF}{maximum value of the log Bayes Factor}
  \item{max.log.K}{value of log(K) where maximum is achieved}
}
\references{
Albert, J. (2013), Looking at Spacings to Access Streakiness, Journal of Quantitative Analysis of Sports, Vol 9, Issue 2, 151-163.
}
\author{
Jim Albert
}

\examples{
y <- c(0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0)
bayes.factor.function(y)
}

