\name{get.poly}
\alias{get.poly}
\title{Get Polygons from a Shapefile
}
\description{It produces an object with polygons that can be used to overprint values on the plot. 
}
\usage{
get.poly(x)
}
\arguments{
  \item{x}{ is the shapefile object.}
}
\details{
}
\value{A maxtirx containing the polygon values.
}
\references{\url{www.metodologiapolitica.com.br}
}
\author{Daniel Marcelino
}
\note{}

\seealso{\code{\link[maptools]{readShapeSpatial}}, \code{\link[maptools]{readShapePoly}}.
}
\examples{

# First example

\dontrun{temp <- tempfile()}

\dontrun{download.file("http://metodologiapolitica.com/download/Brasil.zip",temp)}

\dontrun{shp.br <- maptools::readShapeSpatial(unzip(temp, "Brasil.shp"))}

\dontrun{unlink(temp)}

\dontrun{get.poly(shp.br)}

# Second Example

\dontrun{sids <- maptools::readShapePoly(system.file("shapes/sids.shp", package="maptools") )}

\dontrun{get.poly(sids)}

\dontrun{plot(poly <- get.poly(sids))}

}
\keyword{ Polygons }
\keyword{ Shapefiles }
