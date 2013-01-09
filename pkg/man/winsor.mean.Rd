\encoding{latin1}
\name{winsor.mean}
\alias{winsor.mean}
\title{ Winsorized Mean 
}
\description{The function computes a winsorized mean. Winsorization consists of recoding the top k values.
}
\usage{
winsor.mean(x, k = 1, na.rm = TRUE)
}
\arguments{
  \item{x}{ is the vector to be winsorized.
}
  \item{k}{is an integer for the quantity of outlier elements that should be replaced to the computation purpose.
}
  \item{na.rm}{A logical value indicating whether NA values should be stripped before the computations.
}
}
\details{Winsorizing a vector will produce different results than trimming it. In a trimmed estimator, the extreme values are discarded, while in a Winsorized estimator, the extreme values are instead replaced by certain percentiles (the trimmed minimum and maximum). Note that Winsorization tends to be used for one-variable situations, so it is rarely used in the multivariate
sample survey situation.

}
\value{ An object of the same type as \code{x}.

}
\references{

%TODO include a paper that I red about winsorization and robustness, but could not remember at that time.
Dixon, W. J., and Yuen, K. K. (1999) Trimming and winsorization: A review. \emph{The American Statistician,} \bold{53(3),} 267--269.

Wilcox, R. R. (2012) \emph{Introduction to robust estimation and hypothesis testing.} Academic Press, 30-32.

Statistics Canada (2010) \emph{Survey Methods and Practices.} French version \dQuote{Mèthodes et pratiques d'ènquête.}
}

\author{Daniel Marcelino
}
\note{ Winsorization is a method most common used for one-variable situation. It is rarely used in multivariate analysis.
}


\seealso{\code{\link{detail}}.
}
\examples{
x <- rnorm(100)

winsor.mean(x)

# see this function in context. 
detail(x) 

}

\keyword{ Descriptive Stats }
\keyword{ Average }
\keyword{Winsorization}
\keyword{Outliers}
