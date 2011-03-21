\name{RGEOS Polygon Hole Comment Functions}
\alias{createSPComment}
\alias{createPolygonsComment}

\title{RGEOS Polygon Hole Comment Functions}
\description{Utility functions for assigning ownership of holes to parent polygons}
\usage{
	createSPComment(sppoly,which=NULL,overwrite=TRUE)
	createPolygonsComment(poly)
}

\arguments{
  \item{sppoly}{Object that inherits from the \code{SpatialPolygons} class}
  \item{which}{Vector, which subset of geometries in sppoly should comment attributes be generated}
  \item{overwrite}{Logical, if a comment attribute already exists should it be overwritten}
  \item{poly}{Object of class \code{Polygons}}
}

\details{In order for rgeos to function properly it is necessary that all holes within a given POLYGON or MULTIPOLYGON geometry must belong to a specific polygon. The \code{SpatialPolygons} class implementation does not currently include this information. To work around this limitation rgeos uses an additional comment attribute on the \code{Polygons} class that indicates which hole belongs to which polygon.

Under the current implementation this comment is a text string of numbers separated by spaces where the order of the numbers corresponds to the order of the \code{Polygon} objects in the \code{Polygons} slot of the \code{Polygons} object. A \code{0} implies the \code{Polygon} object is a polygon, a non-zero number implies that the \code{Polygon} object is a hole with the value indicating the index of the \code{Polygon} that "owns" the hole.

\code{createPolygonsComment} attempts to create a valid comment for a \code{Polygons} object by assessing which polygons contain a given hole (using \code{\link{gContains}}). Ownership is assigned to the smallest polygon (by area) that contains the given hole. This is not guaranteed to generate valid polygons, always check the resulting objects for validity.

\code{createSPComment} attempts to create valid comments for all or a subset of polygons within a \code{SpatialPolygons} object.
}

\author{Roger Bivand & Colin Rundel}


\examples{
	
	p1 <- Polygon(cbind(x=c(0, 0, 10, 10, 0), y=c(0, 10, 10, 0, 0)),hole=FALSE) # I
	p2 <- Polygon(cbind(x=c(3, 3, 7, 7, 3), y=c(3, 7, 7, 3, 3)),hole=TRUE) # H
	p8 <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(1, 2, 2, 1, 1)),hole=TRUE) # H
	p9 <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(5, 6, 6, 5, 5)),hole=TRUE) # H
	p3 <- Polygon(cbind(x=c(20, 20, 30, 30, 20), y=c(20, 30, 30, 20, 20)),hole=FALSE) # I
	p4 <- Polygon(cbind(x=c(21, 21, 29, 29, 21), y=c(21, 29, 29, 21, 21)),hole=TRUE) # H
	p5 <- Polygon(cbind(x=c(22, 22, 28, 28, 22), y=c(22, 28, 28, 22, 22)),hole=FALSE) # I
	p6 <- Polygon(cbind(x=c(23, 23, 27, 27, 23), y=c(23, 27, 27, 23, 23)),hole=TRUE) # H
	p7 <- Polygon(cbind(x=c(13, 13, 17, 17, 13), y=c(13, 17, 17, 13, 13)),hole=FALSE) # I
	p10 <- Polygon(cbind(x=c(24, 24, 26, 26, 24), y=c(24, 26, 26, 24, 24)),hole=FALSE) # I
	p11 <- Polygon(cbind(x=c(24.25, 24.25, 25.75, 25.75, 24.25), y=c(24.25, 25.75, 25.75, 24.25, 24.25)),hole=TRUE) # H
	p12 <- Polygon(cbind(x=c(24.5, 24.5, 25.5, 25.5, 24.5), y=c(24.5, 25.5, 25.5, 24.5, 24.5)),hole=FALSE) # I
	p13 <- Polygon(cbind(x=c(24.75, 24.75, 25.25, 25.25, 24.75), y=c(24.75, 25.25, 25.25, 24.75, 24.75)),hole=TRUE) # H
	
	lp <- list(p1, p2, p13, p7, p6, p5, p4, p3, p8, p11, p12, p9, p10)
	#           1   2    3   4   5   6   7   8   9   10   11  12   13
	#           0   1   11   0   6   0   8   0   1   13    0   1    0
	#           I   H    H   I   H   I   H   I   H    H    I   H    I
	pls <- Polygons(lp, ID="1")
	comment(pls)
	
	comment(pls) = createPolygonsComment(pls)
	comment(pls)
	
	
	plot(SpatialPolygons(list(pls)), col="magenta", pbg="cyan")
	title(xlab="Hole slot values before checking")
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

