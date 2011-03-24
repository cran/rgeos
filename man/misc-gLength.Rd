\name{gLength}
\alias{gLength}
\alias{RGEOSLength}

\title{Length of Geometry}
\description{Calculates the length of the given geometry.}
\usage{
	gLength(spgeom, byid=FALSE)
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
}

\value{Returns the length of the geometry in the units of the current projection. By definition [MULTI]POINTs have a length of 0. The length of POLYGONs is the sum of the length of their shell and their hole(s).}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gArea}}
}

\examples{
gLength(readWKT("POINT(1 1)"))

gLength(readWKT("LINESTRING(0 0,1 1,2 2)"))
gLength(readWKT("LINESTRING(0 0,1 1,2 0,3 1)"))

gLength(readWKT("POLYGON((0 0,3 0,3 3,0 3,0 0))"))
gLength(readWKT("POLYGON((0 0,3 0,3 3,0 3,0 0),(1 1,2 1,2 2,1 2,1 1))"))
}

\keyword{spatial}

