\name{RGEOS WKT Functions}
\alias{readWKT}
\alias{writeWKT}

\title{RGEOS WKT Functions}
\description{Functions for reading and writing Well Known Text (WKT)}
\usage{
	readWKT(text, id = NULL, p4s = NULL)
	writeWKT(spgeom, byid = FALSE)
}

\arguments{
  \item{text}{character string of WKT}
  \item{id}{character vector of unique ids to label geometries. Length must match the number of subgeometries in the WKT}
  \item{p4s}{Either a character string or an object of class \code{CRS}}
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
}

\details{readWKT processes the given WKT string and returns an appropriate sp geometry object. If id is not specified then geometries will be labeled by their index position. Because no sp Spatial object may be empty, \code{readWKT} is not permitted to create an empty object.

writeWKT converts an sp geometry object to a GEOS C object which is then written out as a WKT string. If byid is TRUE then each subgeometry is individually converted to a WKT string.}

\references{Additional information on WKT Simple Feature Specification can be found at the following locations:

\url{https://www.ogc.org/standard/sfs/}

\url{https://en.wikipedia.org/wiki/Well-known_text}

\url{https://en.wikipedia.org/wiki/Simple_Features}
}

\author{Colin Rundel}


\examples{
g1=readWKT("POINT(6 10)")
g2=readWKT("LINESTRING(3 4,10 50,20 25)")
g3=readWKT("POLYGON((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2))")
g4=readWKT("MULTIPOINT((3.5 5.6),(4.8 10.5))")
g5=readWKT("MULTILINESTRING((3 4,10 50,20 25),(-5 -8,-10 -8,-15 -4))")
g6=readWKT("MULTIPOLYGON(((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)),((6 3,9 2,9 4,6 3)))")
try(readWKT("POINT EMPTY"))
try(readWKT("MULTIPOLYGON EMPTY"))
g9=readWKT("GEOMETRYCOLLECTION(POINT(4 6),LINESTRING(4 6,7 10))")

writeWKT(g1)
writeWKT(g2)
writeWKT(g3)
writeWKT(g4)
writeWKT(g5)
writeWKT(g6)
writeWKT(g9,byid=FALSE)
writeWKT(g9,byid=TRUE)
}

\keyword{spatial}

