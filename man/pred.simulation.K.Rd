\name{pred.simulation.K}
\alias{pred.simulation.K}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predictive Simulation of Streakiness Assuming Consistent Model
}
\description{
Implements a predictive simulation of the streakiness behavior for a group of binary sequences under a consistent model.  Underlying probabilities of success are estimated using an exchangeable model, binary sequences are simulated using these constant probability values, and the log Bayes factors in support of streakiness are computed for all sequences.
}
\usage{
pred.simulation.K(data, log.K)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
matrix with 2 columns -- first column are the numbers of successes, and the second column contain the corresponding sample sizes
}
  \item{log.K}{
value of log K defining the streaky model for the Bayes test comparing the consistent and streaky models
}
}

\value{
vector of values of the log Bayes factor in support of streakiness
}
\references{
Albert, J. (2013), Looking at Spacings to Access Streakiness, Journal of Quantitative Analysis of Sports, Vol 9, Issue 2, 151-163.
}
\author{
Jim Albert
}

\examples{
y <- rbinom(10, size=500, 
     prob=c(rep(0.1, 3), rep(0.3, 4), rep(0.5, 3)))
data <- cbind(y, 500)
pred.simulation.K(data, 5)
}

