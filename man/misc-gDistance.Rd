\name{gDistance}
\alias{gDistance}
\alias{RGEOSDistance}
\alias{gWithinDistance}
\alias{RGEOSisWithinDistance}
\alias{RGEOSHausdorffDistance}

\title{Distance between geometries}
\description{Calculates the distance between the given geometries}
\usage{
gDistance(spgeom1, spgeom2=NULL, byid=FALSE, hausdorff=FALSE, densifyFrac = NULL)
gWithinDistance(spgeom1, spgeom2=NULL, dist, byid=FALSE,
 hausdorff=FALSE, densifyFrac=NULL)
}

\arguments{
  \item{spgeom1, spgeom2}{sp objects as defined in package sp. If spgeom2 is NULL then spgeom1 is compared to itself.}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{hausdorff}{Logical determining if the discrete Hausdorff distance should be calculated}
  \item{densifyFrac}{Numerical value between 0 and 1 that determines the fraction by which to densify each segment of the geometry.}
  \item{dist}{Numerical value that determines cutoff distance}
}

\value{gDistance by default returns the cartesian minimum distance between the two geometries in the units of the current projection. If \code{hausdorff} is TRUE then the Hausdorff distance is returned for the two geometries.

gWithinDistance returns TRUE if returned distance is less than or equal to the specified \code{dist}.
}

\details{
Discrete Hausdorff distance is essentially a measure of the similarity or dissimilarity of the two geometries, see references below for more detailed explanations / descriptions.

If \code{hausdorff} is TRUE and \code{densifyFrac} is specified then the geometries' segments are densified by splitting each segment into equal length subsegments whose fraction of the total length is equal to \code{densifyFrac}.
} 

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gWithinDistance}}
}

\references{
Hausdorff Differences:
\url{https://en.wikipedia.org/wiki/Hausdorff_distance}
\url{http://lin-ear-th-inking.blogspot.com/2009/01/computing-geometric-similarity.html}
}

\examples{

pt1 = readWKT("POINT(0.5 0.5)")
pt2 = readWKT("POINT(2 2)")

p1 = readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))")
p2 = readWKT("POLYGON((2 0,3 1,4 0,2 0))")

gDistance(pt1,pt2)
gDistance(p1,pt1)
gDistance(p1,pt2)
gDistance(p1,p2)


p3 = readWKT("POLYGON((0 0,2 0,2 2,0 2,0 0))")
p4 = readWKT("POLYGON((0 0,2 0,2 1.9,1.9 2,0 2,0 0))")
p5 = readWKT("POLYGON((0 0,2 0,2 1.5,1.5 2,0 2,0 0))")
p6 = readWKT("POLYGON((0 0,2 0,2 1,1 2,0 2,0 0))")
p7 = readWKT("POLYGON((0 0,2 0,0 2,0 0))")

gDistance(p3,hausdorff=TRUE)
gDistance(p3,p4,hausdorff=TRUE)
gDistance(p3,p5,hausdorff=TRUE)
gDistance(p3,p6,hausdorff=TRUE)
gDistance(p3,p7,hausdorff=TRUE)
}

\keyword{spatial}

