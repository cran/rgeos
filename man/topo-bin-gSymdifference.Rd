\name{gSymdifference}
\alias{gSymdifference}
\title{Geometry Symmetric Difference}
\description{Function for determining the symmetric difference between the two given geometries }
\usage{
gSymdifference(spgeom1, spgeom2, byid=FALSE, id=NULL)
}

\arguments{
  \item{spgeom1, spgeom2}{sp objects as defined in package sp}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
}

\details{Returns the regions of spgeom1 and spgeom2 that do not intersect. If the geometries do not intersect then spgeom1 and spgeom2 will be returned as separate subgeometries.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gDifference}}
\code{\link{gIntersection}}
\code{\link{gUnion}}
}

\examples{
x = readWKT("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
y = readWKT("POLYGON ((5 5, 15 5, 15 15, 5 15, 5 5))")

d = gSymdifference(x,y)
plot(d,col='red',pbg='white')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

