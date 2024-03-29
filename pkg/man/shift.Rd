\name{shift}
\alias{shift}
\alias{lag}
\alias{lead}
\title{Lag and Lead Observations
}
\description{Shift function allows one to either lag or lead a column variables in a data frame.
}
\usage{
shift(x, delta=NA)
}
\arguments{
  \item{x}{ is a variable to be lagged or leaded.
}
  \item{delta}{ an integer value as positive (#) or negative (-#).
}
}
\details{
}
\value{ An object of the same type as \code{x}.
}
\references{\url{http://danielmarcelino.com/SciencePo}
}
\author{Daniel Marcelino
}
\note{}

\seealso{}

\examples{
montreal <- data.frame(
month = a <- c('J','F','M','A','M','J','J','A','S','O','N','D'),
precip = b <- c(78,62,74,78,76, 83, 91, 93, 93,78, 93, 81), 
max = c <- c(-6, -4, 2, 11, 19, 24,  26,  25, 20, 13, 5, -2),
min = d <- c(-15, -13, -7, 1, 8,  13, 16, 14, 9, 3, -2, -10) )

(montreal$L.precip <- shift(montreal$precip, -1) )

(montreal$precip.L <- shift(montreal$precip, 1) )

}

\keyword{ lag }
\keyword{ lead }