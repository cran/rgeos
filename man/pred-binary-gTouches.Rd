\name{gTouches}
\alias{gTouches}
\alias{RGEOSTouches}

\title{Geometry Relationships - Touches}
\description{Functions for testing if the geometries have at least one boundary point in common, but no interior points}
\usage{
	gTouches(spgeom1, spgeom2 = NULL, byid = FALSE, returnDense=TRUE,
 checkValidity=FALSE)
}
	
\arguments{
  \item{spgeom1, spgeom2}{sp objects as defined in package sp. If spgeom2 is NULL then spgeom1 is compared to itself.}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{returnDense}{default TRUE, if false returns a list of the length of spgeom1 of integer vectors listing the \code{1:length(spgeom2)} indices which would be TRUE in the dense logical matrix representation; useful when the sizes of the byid=TRUE returned matrix is very large and it is sparse; essential when the returned matrix would be too large}
  \item{checkValidity}{default FALSE; error meesages from GEOS do not say clearly which object fails if a topology exception is encountered. If this argument is TRUE, \code{gIsValid} is run on each in turn in an environment in which object names are available. If objects are invalid, this is reported and those affected are named}
}

\value{Returns TRUE if the intersection of the boundaries of the two geometries is not empty.}

\note{Error messages from GEOS, in particular topology exceptions, report 0-based object order, so geom 0 is spgeom1, and geom 1 is spgeom2.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gContains}}
\code{\link{gContainsProperly}}
\code{\link{gCovers}}
\code{\link{gCoveredBy}}
\code{\link{gCrosses}}
\code{\link{gDisjoint}}
\code{\link{gEquals}}
\code{\link{gEqualsExact}}
\code{\link{gIntersects}}
\code{\link{gOverlaps}}
\code{\link{gRelate}}
\code{\link{gWithin}}
}

\examples{

	p1 = readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))")
	p2 = readWKT("POLYGON((0 1,0.5 2,1 1,0 1))")
	p3 = readWKT("POLYGON((0.5 1,0 2,1 2,0.5 1))")
	p4 = readWKT("POLYGON((0.5 0.5,0 1.5,1 1.5,0.5 0.5))")
	
	l0 = readWKT("LINESTRING(0 1,0.5 2,1 1)")
	
	l1 = readWKT("LINESTRING(0 0,2 2)")
	l2 = readWKT("LINESTRING(1 1,2 0)")
	l3 = readWKT("LINESTRING(0 2,2 0)")
	
	
	par(mfrow=c(2,3))
	plot(p1,col='blue',border='blue',ylim=c(0,2.5));plot(p2,col='black',add=TRUE,pch=16)
	title(paste("Touches:",gTouches(p1,p2)))
	
	plot(p1,col='blue',border='blue',ylim=c(0,2.5));plot(p3,col='black',add=TRUE,pch=16)
	title(paste("Touches:",gTouches(p1,p3)))
	
	plot(p1,col='blue',border='blue',ylim=c(0,2.5));plot(p4,col='black',add=TRUE,pch=16)
	title(paste("Touches:",gTouches(p1,p4)))
	
	plot(p1,col='blue',border='blue',ylim=c(0,2.5));plot(l0,lwd=2,col='black',add=TRUE,pch=16)
	title(paste("Touches:",gTouches(p1,l0)))
	
	plot(l1,lwd=2,col='blue');plot(l2,lwd=2,col='black',add=TRUE,pch=16)
	title(paste("Touches:",gTouches(l1,l2)))
	
	plot(l1,lwd=2,col='blue');plot(l3,lwd=2,col='black',add=TRUE,pch=16)
	title(paste("Touches:",gTouches(l1,l3)))
	
}
\keyword{spatial}


