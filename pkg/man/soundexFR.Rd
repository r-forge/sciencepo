\encoding{latin1}
\name{soundexFR}
\alias{soundexFR}
\title{French Soundex Encoding
}
\description{Soundex will return the Census Soundex Code for the given Francophone string. The Soundex code consists of up to 4 characters long. The integer refinement will return an integer code value instead.
}
\usage{
soundexFR(term)
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

\seealso{\link[RecordLinkage]{soundex}.
}

\examples{


soundexFR(x <- c("Jean Charest", "John Charest",
	"James Charest", "John James Charest"))

soundexFR(y <- c("GÉRARD DEPARDIEU","LAETITIA CASTA", 
	"ASTRID BERGÈS-FRISBEY", "BRIGITTE BARDOT", 
	"MELANIE LAURENT", "AUDREY TAUTOU") )

}

\keyword{ Soundex }
\keyword{ French }
