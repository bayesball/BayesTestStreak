\name{geometric.plot}
\alias{geometric.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Geometric Probability Plot of a Binary Sequence
}
\description{
Constructs a probability plot to visually see if a group of spacings satisfies a geometric distribution with a constant probability of success.
}
\usage{
geometric.plot(y, ...)
}
\arguments{
  \item{y}{
vector of spacings
}
  \item{\dots}{
any arguments that can be used in the stat.smooth function
}
}

\value{
a geometric probability plot using the ggplot2 package
}
\references{
Albert, J. (2013), Looking at Spacings to Access Streakiness, Journal of Quantitative Analysis of Sports, Vol 9, Issue 2, 151-163.
}
\author{
Jim Albert
}

\examples{
x <- rgeom(500, prob=0.5)
s <- find.spacings(x)
geometric.plot(s$y)
}

