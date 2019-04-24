\name{gIsValid}
\alias{gIsValid}
\alias{RGEOSisValid}

\title{Is Geometry Valid?}
\description{Function tests if the given geometry is valid}
\usage{
gIsValid(spgeom, byid = FALSE, reason=FALSE)
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
  \item{reason}{Logical determining if the function should return a character string describing why the geometry is invalid}
}

\value{By default will return TRUE if the given geometry is well formed, FALSE otherwise. If reason is set to TRUE then a character string is returned describing the geometry, "Valid Geometry" if it is valid or details of the specific issue. Any given geometry may have multiple issues that make it invalid, gIsValid will only return the first, once it has been corrected additionally checking is necessary to confirm that additional issues do not remain.}

\details{Validity is used in reference to 2-dimensional geometries (LINEARRING and [MULTI]POLYGON) whereas Simplicity (\code{\link{gIsSimple}}) is used in reference to 0 and 1-dimensional geometries ([MULTI]POINT and [MULTI]LINESTRING).

A LINEARRING is valid if it does not intersect itself.

A POLYGON is valid if no two rings in the boundary (made up of an exterior ring and interior rings) cross. The boundary of a POLYGON may intersect at a POINT but only as a tangent (i.e. not on a line). A POLYGON may not have cut lines or spikes and the interior rings must be contained entirely within the exterior ring.

A MULTIPOLYGON is valid if and only if all of its elements are valid and the interiors of no two elements intersect. The boundaries of any two elements may touch, but only at a finite number of POINTs.

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
\code{\link{gIsSimple}}
}

\examples{
library(sp)
#LINEARRING Example
l = readWKT("LINEARRING(0 0, 100 100, 100 0, 0 100, 0 0)")
plot(l);title(paste("Valid:",gIsValid(l),"\n",gIsValid(l,reason=TRUE)))

#POLYGON and MULTIPOLYGON Examples
p1 = readWKT("POLYGON ((0 60, 0 0, 60 0, 60 60, 0 60), (20 40, 20 20, 40 20, 40 40, 20 40))")
p2 = readWKT("POLYGON ((0 60, 0 0, 60 0, 60 60, 0 60), (20 40, 20 20, 60 20, 20 40))")
p3 = readWKT(paste("POLYGON ((0 120, 0 0, 140 0, 140 120, 0 120),",
 "(100 100, 100 20, 120 20, 120 100, 100 100),",
 "(20 100, 20 40, 100 40, 20 100))"))

p4 = readWKT("POLYGON ((0 40, 0 0, 40 40, 40 0, 0 40))")
p5 = readWKT("POLYGON ((-10 50, 50 50, 50 -10, -10 -10, -10 50), (0 40, 0 0, 40 40, 40 0, 0 40))")
p6 = readWKT("POLYGON ((0 60, 0 0, 60 0, 60 20, 100 20, 60 20, 60 60, 0 60))")
p7 = readWKT(paste("POLYGON ((40 300, 40 20, 280 20, 280 300, 40 300),",
 "(120 240, 80 180, 160 220, 120 240),",
 "(220 240, 160 220, 220 160, 220 240),",
 "(160 100, 80 180, 100 80, 160 100),",
 "(160 100, 220 160, 240 100, 160 100))"))
p8 = readWKT(paste("POLYGON ((40 320, 340 320, 340 20, 40 20, 40 320),",
 "(100 120, 40 20, 180 100, 100 120),",
 "(200 200, 180 100, 240 160, 200 200),",
 "(260 260, 240 160, 300 200, 260 260),",
 "(300 300, 300 200, 340 320, 300 300))"))
p9 = readWKT(paste("MULTIPOLYGON (((20 380, 420 380, 420 20, 20 20, 20 380),",
 "(220 340, 180 240, 60 200, 200 180, 340 60, 240 220, 220 340)),",
 "((60 200, 340 60, 220 340, 60 200)))"))

par(mfrow=c(3,3))
plot(p1,col='black',pbg='white');title(paste("Valid:",gIsValid(p1),"\n",gIsValid(p1,reason=TRUE)))
plot(p2,col='black',pbg='white');title(paste("Valid:",gIsValid(p2),"\n",gIsValid(p2,reason=TRUE)))
plot(p3,col='black',pbg='white');title(paste("Valid:",gIsValid(p3),"\n",gIsValid(p3,reason=TRUE)))
plot(p4,col='black',pbg='white');title(paste("Valid:",gIsValid(p4),"\n",gIsValid(p4,reason=TRUE)))
plot(p5,col='black',pbg='white');title(paste("Valid:",gIsValid(p5),"\n",gIsValid(p5,reason=TRUE)))
plot(p6,col='black',pbg='white');title(paste("Valid:",gIsValid(p6),"\n",gIsValid(p6,reason=TRUE)))
plot(p7,col='black',pbg='white');title(paste("Valid:",gIsValid(p7),"\n",gIsValid(p7,reason=TRUE)))
plot(p8,col='black',pbg='white');title(paste("Valid:",gIsValid(p8),"\n",gIsValid(p8,reason=TRUE)))
plot(p9,col='black',pbg='white')
title(paste("Valid:",gIsValid(p9),"\n",gIsValid(p9,reason=TRUE)))	
}

\keyword{spatial}

