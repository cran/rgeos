\name{gEquals}
\alias{gEquals}
\alias{RGEOSEquals}
\alias{gEqualsExact}
\alias{RGEOSEqualsExact}

\title{Geometry Relationships - Equality}
\description{Function for testing equivalence of the given geometries}
\usage{
	gEquals(spgeom1, spgeom2 = NULL, byid = FALSE, returnDense=TRUE,
 checkValidity=FALSE)
	gEqualsExact(spgeom1, spgeom2 = NULL, tol=0.0, byid = FALSE,
 returnDense=TRUE, checkValidity=FALSE)
}

\arguments{
  \item{spgeom1, spgeom2}{sp objects as defined in package sp. If spgeom2 is NULL then spgeom1 is compared to itself.}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{tol}{Numerical value of tolerance to use when assessing equivalence}
  \item{returnDense}{default TRUE, if false returns a list of the length of spgeom1 of integer vectors listing the \code{1:length(spgeom2)} indices which would be TRUE in the dense logical matrix representation; useful when the sizes of the byid=TRUE returned matrix is very large and it is sparse; essential when the returned matrix would be too large}
  \item{checkValidity}{default FALSE; error meesages from GEOS do not say clearly which object fails if a topology exception is encountered. If this argument is TRUE, \code{gIsValid} is run on each in turn in an environment in which object names are available. If objects are invalid, this is reported and those affected are named}
}

\value{\code{gEquals} returns TRUE if geometries are "spatially equivalent" which requires that \code{spgeom1} is within \code{spgeom2} and \code{spgeom2} is within \code{spgeom1}, this ignores ordering of points within the geometries. Note that it is possible for geometries with different coordinates to be "spatially equivalent".

\code{gEqualsExact} returns TRUE if geometries are "exactly equivalent" which requires that \code{spgeom1} and \code{spgeom1} are "spatially equivalent" and that their constituent points are in the same order.
}

\note{Error messages from GEOS, in particular topology exceptions, report 0-based object order, so geom 0 is spgeom1, and geom 1 is spgeom2.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gContains}}
\code{\link{gContainsProperly}}
\code{\link{gCovers}}
\code{\link{gCoveredBy}}
\code{\link{gCrosses}}
\code{\link{gDisjoint}}
\code{\link{gEqualsExact}}
\code{\link{gIntersects}}
\code{\link{gOverlaps}}
\code{\link{gRelate}}
\code{\link{gTouches}}
\code{\link{gWithin}}
}


\examples{

# p1 and p2 are spatially identical but not exactly identical due to point ordering
p1=readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))")
p2=readWKT("POLYGON((1 1,0 1,0 0,1 0,1 1))")
p3=readWKT("POLYGON((0.01 0.01,1.01 0.01,1.01 1.01,0.01 1.01,0.01 0.01))")

gEquals(p1,p2)
gEquals(p1,p3)
gEqualsExact(p1,p2)
gEqualsExact(p1,p3,tol=0)
gEqualsExact(p1,p3,tol=0.1)

# pt1 and p2t are spatially identical but not exactly identical due to point ordering
pt1 = readWKT("MULTIPOINT(1 1,2 2,3 3)")
pt2 = readWKT("MULTIPOINT(3 3,2 2,1 1)")
pt3 = readWKT("MULTIPOINT(1.01 1.01,2.01 2.01,3.01 3.01)")

gEquals(pt1,pt2)
gEquals(pt1,pt3)
gEqualsExact(pt1,pt2)
gEqualsExact(pt1,pt3,tol=0)
gEqualsExact(pt1,pt3,tol=0.1)


# l2 contains a point that l1 does not
l1 = readWKT("LINESTRING (10 10, 20 20)")
l2 = readWKT("LINESTRING (10 10, 15 15,20 20)")
gEquals(l1,l2)
gEqualsExact(l1,l2)

}
\keyword{spatial}


