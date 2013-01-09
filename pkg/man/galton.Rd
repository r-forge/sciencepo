\encoding{utf8}
\name{galton}
\alias{galton}
\docType{data}
\title{Galton's Family Data on Human Stature.
}
\description{It is a reproduction of the data set used by Galton in 1885 to study the correlation between the parent's height and their children. Galton introduced the concept of correlation indeed few years later in 1888. He also  suggested the use of the regression line, and was the first to describe the so-called common phenomenon of regression toward the mean, which he first observed in his experiments on the size of the seeds of successive generations of peas. 
}
\usage{data(galton)}
\format{
  A data frame with 928 observations on the following 2 variables.
  \describe{
    \item{\code{parent}}{The parents' average height}
    \item{\code{child}}{The child's height}
  }
}
\details{
}
\source{ 
}
\references{ Francis Galton (1886) Regression Towards Mediocrity in Hereditary Stature. \emph{The Journal of the Anthropological Institute of Great Britain and Ireland,} Vol. \bold{15}, pp. 246--263. 

James A. Hanley (2004) \sQuote{Transmuting} women into men: Galton's family data on human stature. \emph{The American Statistician,} \bold{58(3),} 237-243.
}
\examples{
data(galton)

\dontrun{plot(galton)}

}
\keyword{datasets}
