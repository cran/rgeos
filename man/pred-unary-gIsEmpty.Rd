\name{gIsEmpty}
\alias{gIsEmpty}
\alias{RGEOSisEmpty}

\title{Is Geometry Empty?}
\description{Tests if the given geometry is empty}
\usage{
gIsEmpty(spgeom, byid = FALSE) 
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
}

\value{Returns TRUE if the given geometry is empty, FALSE otherwise.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gIsRing}}
\code{\link{gIsSimple}}
\code{\link{gIsValid}}
}

\examples{
gIsEmpty(readWKT("POINT EMPTY"))
gIsEmpty(readWKT("POINT(1 1)"))
gIsEmpty(readWKT("LINESTRING EMPTY"))
gIsEmpty(readWKT("LINESTRING(0 0,1 1)"))
gIsEmpty(readWKT("POLYGON EMPTY"))
gIsEmpty(readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))"))
}

\keyword{spatial}

