\name{get_streaks}
\alias{get_streaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes lengths of streaks for a binary sequence
}
\description{
Computes the lengths of all streaks of 1's in a vector of binary observations (0's and 1's)
}
\usage{
get_streaks(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of 0's and 1's
}
}

\value{
vector of lengths of streaks
}
\author{
Jim Albert
}

\examples{
y <- c(0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0)
get_streaks(y)
}
