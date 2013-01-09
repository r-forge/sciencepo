\encoding{utf8}
\name{install}
\alias{install}
\title{Install R Packages 
}
\description{Make it easier to install packages
}
\usage{
install(packages = NULL, install = TRUE)
}
\arguments{
  \item{packages}{A package name or a list of packages.}
  \item{install}{If TRUE, the package will be installed into default directory.}
}
\details{To install more than one package, just use concatenate mode \sQuote{c()}.
}
\value{Install packages internally.}
\references{\url{http://danielmarcelino.com/SciencePo} 
}
\author{Daniel Marcelino }
\note{}

\seealso{ \code{\link[xtable]{xtable}}, \code{\link[Zelig]{Zelig}}, \code{\link[descr]{descr}}
}
\examples{

\dontrun{install("SciencePo")}

\dontrun{install(c('xtable', 'Zelig', 'descr' )) }

}

\keyword{ Maintenance }
\keyword{ Packages }
\keyword{ R }
