\name{pbp2016}
\alias{pbp2016}
\docType{data}
\title{Retrosheet Event Data for 2016 Data}
\description{
Retrosheet play-by-play data for selected variables for 2016 season
}
\usage{
pbp2016
}
\format{
  A data frame on the following eight variables.
  \describe{
  \item{GAME_ID}{season}
  \item{AWAY_TEAM_ID}{code of away team}
  \item{INN_CT}{inning number}
  \item{BAT_HOME_ID}{which team is batting}
  \item{BAT_ID}{id code for batter}
  \item{PIT_ID}{id code for pitcher}
  \item{BAT_EVENT_FL}{flag to indicate if a batting event occurred}
  \item{EVENT_CD}{code of play event}
}
}
\source{Retrosheet data}
\keyword{datasets}
