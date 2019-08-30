\name{streak_game_data}
\alias{streak_game_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain game data for streak analysis
}
\description{
Extracts game by game batting data from Retrosheet for a specific player 
}
\usage{
streak_game_data(pbpdata, pid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{pbpdata}{
Retrosheet event file for a particular season or seasons
}
\item{pid}{
Retroid of player
}
}

\value{
data frame with variables Date, AB, H, and Hit
}
\author{
Jim Albert
}

\examples{
}

