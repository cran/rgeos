\name{gArea}
\alias{gArea}
\alias{RGEOSArea}

\title{Area of Geometry}
\description{Calculates the area of the given geometry.}
\usage{
gArea(spgeom, byid=FALSE)
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
}

\value{Returns the area of the geometry in the units of the current projection. By definition non-[MULTI]POLYGON geometries have an area of 0. The area of a POLYGON is the area of its shell less the area of any holes. Note that this value may be different from the \code{area} slot of the \code{Polygons} class as this value does not subtract the area of any holes in the geometry.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gLength}}
}

\examples{
	gArea(readWKT("POINT(1 1)"))
	gArea(readWKT("LINESTRING(0 0,1 1,2 2)"))
	gArea(readWKT("LINEARRING(0 0,3 0,3 3,0 3,0 0)"))
	
	
	p1 = readWKT("POLYGON((0 0,3 0,3 3,0 3,0 0))")
	p2 = readWKT("POLYGON((0 0,3 0,3 3,0 3,0 0),(1 1,2 1,2 2,1 2,1 1))")
	
	gArea(p1)
	p1@polygons[[1]]@area
	
	gArea(p2)
	p2@polygons[[1]]@area
}

\keyword{spatial}

