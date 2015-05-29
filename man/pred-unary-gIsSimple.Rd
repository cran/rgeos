\name{gIsSimple}
\alias{gIsSimple}
\alias{RGEOSisSimple}

\title{Is Geometry Simple?}
\description{Function tests if the given geometry is simple}
\usage{
gIsSimple(spgeom, byid = FALSE) 
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
}

\value{Returns TRUE if the given geometry does not contain anomalous points, such as self intersection or self tangency.}

\details{Simplicity is used in reference to 0 and 1-dimensional geometries ([MULTI]POINT and [MULTI]LINESTRING) whereas Validity (\code{\link{gIsValid}}) is used in reference to 2-dimensional geometries ([MULTI]POLYGON). 

A POINT is always simple.

A MULTIPOINT is simple if no two points are identical.

A LINESTRING is simple if it does not pass through the same point twice (self intersection) except at the end points, in which case it is a ring (\code{\link{gIsRing}}).

A MULTILINESTRING is simple if all of its subgeometries are simple and none of the subgeometries intersect except at end points.

A [MULTI]POLYGON is simple by definition.

Many of the functions in rgeos expect simple/valid geometries and may exhibit unpredictable behavior if given an invalid geometry. Checking of validity/simplicity can be computationally expensive for complex geometries and so is not done by default, any new geometries should be checked.
}


\references{
Validity Details:
\url{http://postgis.net/docs/manual-2.0/using_postgis_dbmanagement.html#OGC_Validity}
}



\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gIsEmpty}}
\code{\link{gIsRing}}
\code{\link{gIsValid}}
}

\examples{
# MULTIPOINT examples
gIsSimple(readWKT("MULTIPOINT(1 1,2 2,3 3)"))
gIsSimple(readWKT("MULTIPOINT(1 1,2 2,1 1)"))

# LINESTRING examples
l1 = readWKT("LINESTRING(0 5,3 4,2 3,5 2)")
l2 = readWKT("LINESTRING(0 5,4 2,5 4,0 1)")
l3 = readWKT("LINESTRING(3 5,0 4,0 2,2 0,5 1,4 4,4 5,3 5)")
l4 = readWKT("LINESTRING(3 5,0 4,4 3,5 2,3 0,1 2,4 5,3 5)")

par(mfrow=c(2,2))
plot(l1);title(paste("Simple:",gIsSimple(l1)))
plot(l2);title(paste("Simple:",gIsSimple(l2)))
plot(l3);title(paste("Simple:",gIsSimple(l3)))
plot(l4);title(paste("Simple:",gIsSimple(l4)))

# MULTILINESTRING examples
ml1 = readWKT("MULTILINESTRING((0 5,1 2,5 0),(3 5,5 4,4 1))")
ml2 = readWKT("MULTILINESTRING((0 5,1 2,5 0),(0 5,5 4,4 1))")
ml3 = readWKT("MULTILINESTRING((0 5,1 2,5 0),(3 5,5 4,2 0))")

par(mfrow=c(1,3))
plot(ml1);title(paste("Simple:",gIsSimple(ml1)))
plot(ml2);title(paste("Simple:",gIsSimple(ml2)))
plot(ml3);title(paste("Simple:",gIsSimple(ml3)))	
}

\keyword{spatial}

