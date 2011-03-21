\name{gIntersects}
\alias{gIntersects}
\alias{RGEOSIntersects}
\alias{gDisjoint}
\alias{RGEOSDisjoint}

\title{Geometry Relationships - Intersects and Disjoint}
\description{Function for testing if the geometries have at least one point in common or no points in common}
\usage{
	gIntersects(spgeom1, spgeom2 = NULL, byid = FALSE, prepared=TRUE)
	gDisjoint(spgeom1, spgeom2 = NULL, byid = FALSE)
}
	
\arguments{
  \item{spgeom1, spgeom2}{sp objects as defined in package sp. If spgeom2 is NULL then spgeom1 is compared to itself.}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{prepared}{Logical determining if prepared geometry (spatially indexed) version of the GEOS function should be used. In general prepared geometries should be faster than the alternative.}
}

\value{\code{gIntersects} returns TRUE if \code{spgeom1} and \code{spgeom2} have at least one point in common.

\code{gDisjoint} returns TRUE if \code{spgeom1} and \code{spgeom2} have no points in common.
}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gContains}}
\code{\link{gContainsProperly}}
\code{\link{gCovers}}
\code{\link{gCoveredBy}}
\code{\link{gCrosses}}
\code{\link{gEquals}}
\code{\link{gEqualsExact}}
\code{\link{gOverlaps}}
\code{\link{gRelate}}
\code{\link{gTouches}}
\code{\link{gWithin}}
}

\examples{
	p1 = readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))")
	p2 = readWKT("POLYGON((0.5 1,0 2,1 2,0.5 1))")
	p3 = readWKT("POLYGON((0.5 0.5,0 1.5,1 1.5,0.5 0.5))")

	l1 = readWKT("LINESTRING(0 3,1 1,2 2,3 0)")
	l2 = readWKT("LINESTRING(1 3.5,3 3,2 1)")
	l3 = readWKT("LINESTRING(-0.1 0,-0.1 1.1,1 1.1)")
	
	pt1 = readWKT("MULTIPOINT(1 1,3 0,2 1)")
	pt2 = readWKT("MULTIPOINT(0 3,3 0,2 1)")
	pt3 = readWKT("MULTIPOINT(-0.2 0,1 -0.2,1.2 1,0 1.2)")

	
	par(mfrow=c(3,2))
	plot(p1,col='blue',border='blue',ylim=c(0,2.5));plot(p2,col='black',add=TRUE,pch=16)
	title(paste("Intersects:",gIntersects(p1,p2),
				"\nDisjoint:",gDisjoint(p1,p2)))
	
	plot(p1,col='blue',border='blue',ylim=c(0,2.5));plot(p3,col='black',add=TRUE,pch=16)
	title(paste("Intersects:",gIntersects(p1,p3),
				"\nDisjoint:",gDisjoint(p1,p3)))
				
	plot(l1,col='blue');plot(pt1,add=TRUE,pch=16)
	title(paste("Intersects:",gIntersects(l1,pt1),
				"\nDisjoint:",gDisjoint(l1,pt1)))
	
	plot(l1,col='blue');plot(pt2,add=TRUE,pch=16)
	title(paste("Intersects:",gIntersects(l1,pt2),
				"\nDisjoint:",gDisjoint(l1,pt2)))
	
	plot(p1,col='blue',border='blue',xlim=c(-0.5,2),ylim=c(0,2.5));plot(l3,lwd=2,col='black',add=TRUE)
	title(paste("Intersects:",gIntersects(p1,l3),
				"\nDisjoint:",gDisjoint(p1,l3)))
	
	plot(p1,col='blue',border='blue',xlim=c(-0.5,2),ylim=c(-0.5,2));plot(pt3,pch=16,col='black',add=TRUE)
	title(paste("Intersects:",gIntersects(p1,pt3),
				"\nDisjoint:",gDisjoint(p1,pt3)))
}
\keyword{spatial}


