\name{geometric_plot_compare}
\alias{geometric_plot_compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Geometric Probability Plot Comparing Binary Sequences
}
\description{
Constructs probability plots for several players to visually see if the spacings satisfies a geometric distribution with a constant probability of success.
}
\usage{
geometric_plot_compare(spacing_data)
}
\arguments{
  \item{spacing_data}{
data frame with components
 \item{N}{index}
 \item{Outcome}{spacings}
 \item{BAT_ID}{player id}
}
}
}

\value{
parallel geometric probability plots using the ggplot2 package
}
\references{
Albert, J. (2013), Looking at Spacings to Access Streakiness, Journal of Quantitative Analysis of Sports, Vol 9, Issue 2, 151-163.
}
\author{
Jim Albert
}

\examples{
spacings1 <- data.frame(N = 1:500,
                   Spacing = rgeom(500, prob = 0.3),
                   BAT_ID = "player1")
spacings2 <- data.frame(N = 1:500,
                   Spacing = rgeom(500, prob = 0.6),
                   BAT_ID = "player2")
spacings_data <- rbind(spacings1, spacings2)        
geometric_plot_compare(spacings_data)
}

