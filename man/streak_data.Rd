\name{streak_data}
\alias{streak_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain binary data for streak analysis
}
\description{
Extracts batting data from Retrosheet for a specific player and specific event
}
\usage{
streak_data(pbpdata, pid, eventcode, AB=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pid}{
Retroid of player
}
\item{pbpdata}{
Retrosheet event file for a particular season or seasons
}
\item{eventcode}{
Code(s) for variable EVENT_CD that indicates event (can use "H", "HR", "SO", "OB", or sets of numeric codes)
}
\item{AB}{
Should one only consider official at-bats?
}
}

\value{
data frame of 0-1 data for all plate appearances in time
}
\author{
Jim Albert
}

\examples{
}

