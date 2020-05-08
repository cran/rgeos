\name{gInterpolate}
\alias{gInterpolate}
\title{Interpolate Points along Line Geometry}
\usage{
gInterpolate(spgeom, d, normalized = FALSE)
}
\arguments{
\item{spgeom}{SpatialLines or SpatialLinesDataFrame object}

\item{d}{Numeric vector specifying the distance along the line geometry}

\item{normalized}{Logical determining if normalized distances
should be used}
}
\value{
SpatialPoints object
}
\description{
Return points at specified distances along a line.
}
\details{
If \code{normalized=TRUE}, the distances will be interpreted
  as fractions of the line length.
}
\examples{
gInterpolate(readWKT("LINESTRING(25 50, 100 125, 150 190)"),
             d=seq(0, 1, by = 0.2), normalized = TRUE)
}
\seealso{
gInterpolate
}
\author{
Rainer Stuetz
}
\keyword{spatial}
