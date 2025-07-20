\name{moving_average}
\alias{moving_average}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moving Average Plot
}
\description{
Constructs a moving average plot of a binary sequence.
}
\usage{
moving_average(streak_data, width=20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{streak_data}{
data frame with components N and Outcome
}
  \item{width}{
size of the window for the moving average
}
}
\value{
data frame with additional variables
\item{Index}{moving average of index}
 \item{Average}{moving average of outcome}
 \item{AVG}{overall average}
}

\references{
Albert, J. (2013), Looking at Spacings to Access Streakiness, Journal of Quantitative Analysis of Sports, Vol 9, Issue 2, 151-163.
}
\author{
Jim Albert
}

\examples{
streak_data <- data.frame(N = 1:500,
              Outcome = rbinom(500, size=1, prob=0.3))
moving_average(streak_data)
}

