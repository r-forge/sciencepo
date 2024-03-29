\name{anonymize}
\alias{anonymize}
\title{To Make Anonymous a Data Frame
}
\description{Replaces factor and character variables by a combination of letters and numbers. Numeric columns are also transformed, see details.
}
\usage{
anonymize(x)
}
\arguments{
  \item{x}{A vector or a data frame.}
}

\details{}

\value{ An object of the same type as \code{x}.
}

\references{}

\author{Daniel Marcelino
}
\note{}

\seealso{\code{\link{soundexBR}}.
}

\examples{

mtl <- data.frame(
month = a <- c('J','F','M','A','M','J','J','A','S','O','N','D'),
precip = b <- c(78,62,74,78,76, 83, 91, 93, 93,78, 93, 81), 
max = c <- c(-6, -4, 2, 11, 19, 24,  26,  25, 20, 13, 5, -2),
min = d <- c(-15, -13, -7, 1, 8,  13, 16, 14, 9, 3, -2, -10) )

anonymize(mtl)

}

\keyword{ anonymously }
\keyword{ deform }
\keyword{ randomize }