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
plot_streak_data(y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
vector of 0's and 1's
}
}

\author{
Jim Albert
}

\examples{
y <- rbinom(500, size=1, prob=0.3)
plot_streak_data(y)
}

