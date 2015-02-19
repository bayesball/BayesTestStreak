\name{bayes.factor.K}
\alias{bayes.factor.K}

\title{
Computes Bayes Factor for Single Alternative
}
\description{
Computes Bayes factor in support of a streaky model over a consistent model for a single alternative for the streaky model.
}
\usage{
bayes.factor.K(y, log.K)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
vector of binary outcomes (0's and 1's)
}
  \item{log.K}{
value of log(K) under streaky model
}
}

\value{
log Bayes factor in support of streaky model
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
