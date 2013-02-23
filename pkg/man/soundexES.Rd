\encoding{latin1}
\name{soundexES}
\alias{soundexES}
\title{Spanish Soundex Encoding
}
\description{Soundex will return the Census Soundex Code for the given Francophone string. The Soundex code consists of up to 4 characters long. The integer refinement will return an integer code value instead.
}
\usage{
soundexES(term)
}
\arguments{
  \item{term}{is the vector with character strings.
}
}
\details{
}
\value{ A character vector or matrix with the same dimensions as \code{term}.
}
\references{
Borg, Andreas and Murat Sariyar. (2012) \emph{RecordLinkage: Record Linkage in R,} R package version 0.4-1,  \url{http://CRAN.R-project.org/package=RecordLinkage}.

}
\author{Daniel Marcelino
}
\note{This function is an adaptation of the US census soundex version. See in \url{http://www.archives.gov/research/census/soundex.html}
}

\seealso{\code{\link{soundexBR}}, \code{\link{soundexFR}} .
}

\examples{

soundexES(x <- c('Alejandro Angel Tapias', 'Alejandro Tapias',
'Alejandro A. Tapias', 'Alexander Angel Tapias',
'Alexandre Angel Tapias'))

\dontrun{soundexES(y <- c('Beatriz Rivera', 'Carol Galais', 'Evita Perón', 
'Irene Martin') )
}
}

\keyword{ soundex }
\keyword{ Spanish }
\keyword{ RecordLinkage  }
