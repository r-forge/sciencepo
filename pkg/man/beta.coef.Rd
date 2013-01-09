\encoding{utf8}
\name{beta.coef}
\alias{beta.coef}
\alias{standardize}
\title{Standardized Beta Coefficients
}
\description{Produces standardized beta coefficients from an adjusted model. Standardized coefficients refer to how many standard deviations a dependent variable will change per standard deviation increase in the predictor variable.
}
\usage{
beta.coef(md)
}
\arguments{
  \item{md}{is a linear object model.
  }
}
\details{Standardized beta coefficients also known as beta coefficients are estimated from unstandardized coefficients, partially adjusted by the quotient of standard deviation of independent variable, and the standard deviation of dependent variable. Basically, beta coefficients report the relative importance of each independent variables. The procedure of coefficient standardization is usually done in multiple regression analysis. It helps to answer the question of which of the independent variables have the greater effect on the dependent variable. Standardization is specially useful when variables are measured in different units of measurement.
}
\value{ a numeric value of standardized beta coefficient.

}
\references{
Baguley, T. (2009) Standardized or simple effect size: What should be reported?. \emph{British Journal of Psychology,} \bold{100(3),} 603-617.

Dallal, J. Which Predictors Are More Important? \url{http://www.jerrydallal.com/LHSP/importnt.htm}

Gelman, A., & Hill, J. (2006) \emph{Data analysis using regression and multilevel/hierarchical models.} Cambridge University Press.
}
\author{Daniel Marcelino
}
\note{To be fair, there are cons and pros of using standardized beta coefficients, see for example, positive arguments in Gelman's book and counterarguments in Dallal's page.
}


\seealso{ \code{\link{galton}}
}
\examples{
data(galton) #use some data

#fit a linear regression (OLS) model.

(model <- lm(child ~ parent, data = galton) )

#finally, apply the standardization onto model's coefficients.

beta.coef(model)

}

\keyword{ Coefficients }
\keyword{Standardized }
