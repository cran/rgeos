\name{gDelaunayTriangulation}
\alias{gDelaunayTriangulation}

\title{Compute Delaunay triangulation between points}
\description{Function to compute the Delaunay triangulation between points; only available for GEOS >= 3.4.0.}
\usage{
	gDelaunayTriangulation(spgeom, tolerance=0.0, onlyEdges=FALSE)
}

\arguments{
  \item{spgeom}{sp points object as defined in package sp}
  \item{tolerance}{Numerical tolerance value to be used in triangulation}
  \item{onlyEdges}{Logical, default returns triangles as polygons, if TRUE, returns a SpatialLines object with a single MULTILINESTRING}
}

\value{Either a SpatialPolygons object or a SpatialLines object containing a single Lines object of the undirected edges in the triangulation.}

\details{When onlyEdges is TRUE, the SpatialLines object may be de-merged to identify the input points that are touched by each edge, making it possible to identify spatial neighbours.}

\author{Roger Bivand}

\references{
\url{https://en.wikipedia.org/wiki/Delaunay_triangulation}
}


\examples{
if (version_GEOS0() > "3.4.0") {
library(sp)
data(meuse)
coordinates(meuse) <- c("x", "y")
plot(gDelaunayTriangulation(meuse))
points(meuse)
out <- gDelaunayTriangulation(meuse, onlyEdges=TRUE)
lns <- slot(slot(out, "lines")[[1]], "Lines")
out1 <- SpatialLines(lapply(seq(along=lns), function(i) Lines(list(lns[[i]]),
 ID=as.character(i))))
out2 <- lapply(1:length(out1), function(i) which(gTouches(meuse, out1[i],
 byid=TRUE)))
out3 <- do.call("rbind", out2)
o <- order(out3[,1], out3[,2])
out4 <- out3[o,]
out5 <- data.frame(from=out4[,1], to=out4[,2], weight=1)
head(out5)
\dontrun{
if (require(spdep)) {
class(out5) <- c("spatial.neighbour", class(out5))
attr(out5, "n") <- length(meuse)
attr(out5, "region.id") <- as.character(1:length(meuse))
nb1 <- sn2listw(out5)$neighbours
nb2 <- make.sym.nb(nb1)
}
}
}
}

\keyword{spatial}
