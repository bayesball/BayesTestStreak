\name{bayes_factor_logK}
\alias{bayes_factor_logK}

\title{
Computes Bayes Factor for Several Alternatives
}
\description{
Computes Bayes factor in support of a streaky model over a consistent model for a group of alternatives for the streaky model.
}
\usage{
bayes_factor_logK(streak_data, log_K)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{streak_data}{
data frame with components N and Outcome}   
  \item{log_K}{
vector of values of log(K) under streaky model
}}

\value{
vector of log Bayes factors in support of streaky model
}
\references{
Albert, J. (2013), Looking at Spacings to Access Streakiness, Journal of Quantitative Analysis of Sports, Vol 9, Issue 2, 151-163.
}
\author{
Jim Albert
}

\examples{
streak_data <- data.frame(N = 1:13,
   Outcome = c(0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0))
bayes_factor_logK(streak_data)
}
