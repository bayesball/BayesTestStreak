\name{mavg_plot}
\alias{mavg_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moving Average Plot
}
\description{
Constructs a moving average plot of a binary sequence.
}
\usage{
mavg_plot(y, width=20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
vector of 0's and 1's
}
  \item{width}{
size of the window for the moving average
}
}

\references{
Albert, J. (2013), Looking at Spacings to Access Streakiness, Journal of Quantitative Analysis of Sports, Vol 9, Issue 2, 151-163.
}
\author{
Jim Albert
}

\examples{
y <- rbinom(500, size=1, prob=0.3)
mavg_plot(y, width=20)
}

