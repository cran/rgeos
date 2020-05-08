\name{gProject}
\alias{gProject}
\title{Project Points to Line Geometry}
\usage{
gProject(spgeom, sppoint, normalized = FALSE)
}
\arguments{
\item{spgeom}{SpatialLines or SpatialLinesDataFrame object}

\item{sppoint}{SpatialPoints or SpatialPointsDataFrame object}

\item{normalized}{Logical determining if normalized distances
should be used}
}
\value{
a numeric vector containing the distances along the line to
  points nearest to the specified points
}
\description{
Return distances along geometry to points nearest the specified points.
}
\details{
If \code{normalized=TRUE}, distances normalized to the length
  of the geometry are returned, i.e., values between 0 and 1.
}
\examples{
l <- readWKT("LINESTRING(0 1, 3 4, 5 6)")
p1 <- readWKT("MULTIPOINT(3 2, 3 5)")
frac <- gProject(l, p1)
p2 <- gInterpolate(l, frac)
plot(l, axes=TRUE)
plot(p1, col = "blue", add = TRUE)
plot(p2, col = "red", add = TRUE)
}
\seealso{
gInterpolate
}
\author{
Rainer Stuetz
}
\keyword{spatial}
