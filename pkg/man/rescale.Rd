\name{rescale}
\alias{rescale}
\title{Rescaling as Z-scores and Centering 
}
\description{Provides a rapidly way to generate standard variables. 
}
\usage{
rescale(x, fun = "center")
}
\arguments{
  \item{x}{ A vector to be computed
}
  \item{fun}{A function to be applied: 'z-score', the defaut is 'center'. 
}
}
\details{
}
\value{An object the same class as \code{x}.
}
\references{
}
\author{Daniel Marcelino
}
\note{
}


\seealso{
}
\examples{
x <- sample(10)

rescale(x)

}
\keyword{ centre mean }
\keyword{ z-scores }
\keyword{standardization }

