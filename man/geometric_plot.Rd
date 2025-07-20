\name{geometric_plot}
\alias{geometric_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Geometric Probability Plot of a Binary Sequence
}
\description{
Constructs a probability plot to visually see if a group of spacings satisfies a geometric distribution with a constant probability of success.
}
\usage{
geometric_plot(spacings_data)
}
\arguments{
 \item{spacings_data}{
data frame with components
 \item{N}{index}
 \item{Outcome}{spacings}
}
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
spacings_data <- data.frame(N = 1:500,
                   Spacing = rgeom(500, prob = 0.5))
geometric_plot(spacings_data)
}

