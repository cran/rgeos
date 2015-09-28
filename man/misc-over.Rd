\name{over}
\alias{overGeomGeom}
\alias{overGeomGeomDF}

\title{Find spatial join or intersections}
\description{Find spatial join or intersections}
\usage{
overGeomGeom(x, y, returnList = FALSE, fn = NULL, ..., minDimension = -1)
overGeomGeomDF(x, y, returnList = FALSE, fn = NULL, ..., minDimension = -1)
}

\arguments{
  \item{x}{see \link[sp]{over}}
  \item{y}{see \link[sp]{over}}
  \item{returnList}{see \link[sp]{over}}
  \item{fn}{see \link[sp]{over}}
  \item{...}{see \link[sp]{over}}
  \item{minDimension}{integer; if \code{-1}, \link{gIntersects} is used to find
  geometry intersections; if \code{0}, \code{1} or \code{2}, \link{gRelate} is used. if
  \link{gRelate} is used, intersecting geometries are ordered by the dimension of the
  intersection (2: area overlap; 1: line in common; 0: point in common), and \code{minDimension}
  determines the mininum dimension of intersection required. }
}

\value{see \link[sp]{over}}
\note{\link{gRelate} (\code{minDimension} > -1) is likely to be substantially slower than
\link{gIntersects}.}

\author{Edzer Pebesma}
\examples{
p1 = readWKT("POLYGON((0 1,0.95 0.31,0.59 -0.81,-0.59 -0.81,-0.95 0.31,0 1))")
p2 = readWKT("POLYGON((2 2,-2 2,-2 -2,2 -2,2 2),(1 1,-1 1,-1 -1,1 -1,1 1))")

overGeomGeom(p1,p2)
}
\keyword{spatial}
