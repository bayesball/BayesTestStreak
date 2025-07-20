\name{random_streak}
\alias{random_streak}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graph of Streaky Patterns in Coin Tossing
}
\description{
Constructs a histogram of a streaky stat applied to ofers for repeated flips of a coin.
}
\usage{
random_streak(n = 50, p = 0.5, iter = 1000,
                          stat = sd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
number of coin flips
}
  \item{width}{
probability of heads in each flip
}
\item{iter}{
number of simulations
}
\item{stat}{
function to be applied to the sequence of ofers
}
}

\author{
Jim Albert
}

\examples{
random_streak(n = 40, p = 0.8)
}

