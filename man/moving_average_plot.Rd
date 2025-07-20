\name{moving_average_plot}
\alias{moving_average_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moving Average Plot
}
\description{
Constructs a moving average plot.
}
\usage{
moving_average_plot(moving_avg_df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{moving_avg_df}{
data frame of moving averages
}
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
ma <- moving_average(streak_data, width = 50)
moving_average_plot(ma)
}

