\name{gDifference}
\alias{gDifference}
\title{Geometry Difference}
\description{Function for determining the difference between the two given geometries.}
\usage{
gDifference(spgeom1, spgeom2, byid=FALSE, id=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spgeom1, spgeom2}{sp objects as defined in package sp}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
}

\details{Returns the regions of spgeom1 that are not within spgeom2. If the geometries do not intersect then the result is just spgeom1. Note that the function is not symmetric for spgeom1 and spgeom2.}


\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gIntersection}}
\code{\link{gSymdifference}}
\code{\link{gUnion}}
}

\examples{
x = readWKT("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
y = readWKT("POLYGON ((3 3, 7 3, 7 7, 3 7, 3 3))")

d = gDifference(x,y)
plot(d,col='red',pbg='white')

# Empty geometry since y is completely contained with x
d2 = gDifference(y,x)
}

\keyword{spatial}

