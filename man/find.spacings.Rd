\name{find.spacings}
\alias{find.spacings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes spacings for a binary sequence
}
\description{
Computes the spacings or gaps between successes in a vector of binary observations (0's and 1's)
}
\usage{
find.spacings(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of 0's and 1's
}
}

\value{
A list with components
  \item{y}{vector of spacings}
 \item{I}{vector indicating if spacings are complete or right censored}
}
\references{
Albert, J. (2013), Looking at Spacings to Access Streakiness, Journal of Quantitative Analysis of Sports, Vol 9, Issue 2, 151-163.
}
\author{
Jim Albert
}

\examples{
y <- c(0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0)
find.spacings(y)
}
