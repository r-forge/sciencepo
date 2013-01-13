\encoding{latin1}
\name{accent}
\alias{accent}

\title{Remove Accent Marks}

\description{Replace lower and upper case accented letters with their counterpart without the diacritics.
}
\usage{
accent(x)
}
\arguments{
  \item{x}{Is a data object or a vector. }
}
\details{
}
\value{ An object of the same type as \code{x}.

}
\references{\url{http://danielmarcelino.com/SciencePo} 

}
\author{Daniel Marcelino
}
\note{
}

\seealso{\code{\link{rm.tags}}
}

\examples{
accent("Não me julgues pela metade se não me conheces por inteiro.")

accent("Voilà, l'hétérogénéité réapparaître")

# generate some data

x <- rep(c("Patrik Öhberg", "Språkrådet", "Marie-Françoise",
"Thérèse", "Frédérique", "Anaëlle", "Anaïs", "Sélène"), 3)


accent(x)

}
\keyword{ Accent Marks }
\keyword{ Strings }
\keyword{ Diacritics }

