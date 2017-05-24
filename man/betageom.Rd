\name{betageom}
\alias{betageom}

\title{
Computes Log Posterior for Beta/Geometric Model
}
\description{
Computes log posterior of logit mean and log precision for beta geometric model
}
\usage{
betageom(theta, s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
vector of logit mean and log precision
}
  \item{s}{
list of streak data with components y and I
}
}

\value{
value of log posterior
}
\author{
Jim Albert
}
\examples{
y <- c(0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0)
sp <- find.spacings(y)
betageom(c(0, 0), sp)
}
