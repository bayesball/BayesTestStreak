\name{plot_streak_data}
\alias{plot_streak_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rug plot of binary data
}
\description{
Constructs a rug plot of a binary sequence.
}
\usage{
plot_streak_data(streak_data, title = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{streak_data}{
    data frame with components
 \item{N}{index}
 \item{Outcome}{binary observations}
}
\item{title}
  title to put on graph
}

\author{
Jim Albert
}

\examples{
streak_data <- data.frame(N = 1:500,
Outcome = rbinom(500, size=1, prob=0.3))
plot_streak_data(streak_data)
}

