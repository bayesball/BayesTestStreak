\name{find_spacings}
\alias{find_spacings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes spacings for a binary sequence
}
\description{
Computes the spacings or gaps between successes in binary observations (0's and 1's)
}
\usage{
find.spacings(streak_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{streak_data}{
data frame with components
 \item{N}{index}
 \item{Outcome}{binary observations}
}
}

\value{
A data frame with components
 \item{N}{index}
 \item{Spacing}{spacing}
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
find_spacings(streak_data)
}
