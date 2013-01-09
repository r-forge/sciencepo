\encoding{latin1}
\name{rm.tags}
\alias{rm.tags}
\title{Remove String Tags
}
\description{Clean up x of string tags
}
\usage{
rm.tags(x, tab = FALSE)
}
\arguments{
  \item{x}{is a vector or a data frame containing string tags.}
  \item{tab}{a logical value indicating whether tab spaces should be treated as extraneous characters.}
}
\details{}
\value{An object of the same type as \code{x}.
}
\references{\url{http://danielmarcelino.com/SciencePo}
}
\author{Daniel Marcelino}
\note{}

\seealso{ \code{\link{accent}}.
}
\examples{

tags <- "Não me julgues pela metade & # \%	."

rm.tags(tags)

# compare with the accent function

accent(rm.tags(tags))
}

\keyword{ Trim }
\keyword{ Accent marks }
\keyword{ Tags}
