\encoding{latin1}
\name{SciencePo-package}
\alias{SciencePo}
\docType{package}
\title{Tools for Research in Political Science
}
\description{SciencePo is a collection of functions for making the use of R in political science even more intuitive. SciencePo covers standard descriptive analyses for observational and experimental data. Later updates will focus on cutting-edge methods published on the main journals of the field.
}
\details{
\tabular{ll}{
Package: \tab SciencePo\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2012-12-01\cr
License: \tab LGPL (>= 2.0, < 3)\cr }
}

\author{Daniel Marcelino <dmarcelino@live.com>
}
\references{

Marcelino, Daniel. (2010) \emph{Sobre dinheiro e eleições: um estudo dos gastos de campanha para o Congresso Nacional em 2002 e 2006.}
}

\keyword{ Descriptive Stats }
\keyword{ Experiments }
\keyword{ Political systems }
\keyword{ Simulations }

\seealso{ \code{\link{detail}},  \code{\link{shift}},\code{\link{gsr}}, \code{\link{soundexBR}}, \code{\link{tableau}}, \code{\link{accent}}, \code{\link{beta.coef}}, \code{\link{outliers}}, \code{\link{anonymize}}, \code{\link{lsq}}, \code{\link{get.poly}}, \code{\link{rprob}}, \code{\link{rstack}}, \code{\link{winsor.mean}}, \code{\link{get.ci}}, \code{\link{get.ipc}}. 
}

\examples{
# Loading some data 

montreal <- data.frame(
month = a <- c('J','F','M','A','M','J','J','A','S','O','N','D'),
precip = b <- c(78,62,74,78,76, 83, 91, 93, 93,78, 93, 81), 
max = c <- c(-6, -4, 2, 11, 19, 24,  26,  25, 20, 13, 5, -2),
min = d <- c(-15, -13, -7, 1, 8,  13, 16, 14, 9, 3, -2, -10) )

detail(montreal) 

data(galton)

(model <- lm(child ~ parent, data = galton) )

beta.coef(model)

anonymize(montreal)

outliers(galton)

rprob(nerlove63)

rstack(rprob(nerlove63))

lsq(4, 3, seed = 123)


get.ci(sample = 800, n = 1, mu = .29)


sids <- maptools::readShapePoly(system.file("shapes/sids.shp",
package="maptools") )

get.poly(sids)

plot(poly <- get.poly(sids))


}