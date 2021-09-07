\name{gMakeValid}
\alias{gMakeValid}

\title{Make invalid geometries valid}
\description{Function returns a valid geometry, not available before GEOS 3.8.0; from 3.10.0 two correction strategies}
\usage{
gMakeValid(spgeom, byid=FALSE, id = NULL, original=NULL, keepCollapsed=NULL)
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
  \item{original}{default NULL; if GEOS < 3.10.0, TRUE, if GEOS >= 3.10.0, either set directly or taken from environment variable \code{GEOS_MAKE_VALID} with values \code{LINEWORK} for the original algorithm or \code{STRUCTURE} for the new algorithm. If \code{GEOS_MAKE_VALID} is \code{STRUCTURE}, \code{original} is FALSE}
  \item{keepCollapsed}{default NULL; If GEOS >= 3.10.0 and \code{original} is FALSE, the argument is used to control whether objects with dimension reduction are discarded (keepCollapsed is FALSE) or retained (TRUE). It may be set directly, or if NULL controlled by environment variable \code{GEOS_MAKE_VALID_KEEPCOLLAPSED} with values \code{FALSE} or \code{TRUE}. If \code{GEOS_MAKE_VALID_KEEPCOLLAPSED} is \code{TRUE}, \code{keepCollapsed} is TRUE}
}

\details{Returns a valid geometry or collection of geometries of different types. For details on the buffered geometry fixer, see links from \url{https://github.com/r-spatial/sf/issues/1655}.}

\author{Roger Bivand}

\seealso{\link{SpatialCollections-class}}


\examples{
# Based on test geometries from sf
run <- FALSE
if (version_GEOS0() >= "3.8.0") run <- TRUE
if (run) {
X <- readWKT("POLYGON ((0 0, 0.5 0, 0.5 0.5, 0.5 0, 1 0, 1 1, 0 1, 0 0))")
gIsValid(X)
}
if (run) {
class(X)
}
if (run) {
row.names(X)
}
if (run) {
Y <- gMakeValid(X)
}
if (run) {
gIsValid(Y)
}
if (run) {
class(Y)
}
if (run) {
plot(slot(Y, "polyobj"))
plot(slot(Y, "lineobj"), add=TRUE, col="red")
}
if (run) {
row.names(slot(Y, "polyobj"))
}
if (run) {
row.names(slot(Y, "lineobj"))
}
run <- FALSE
if (version_GEOS0() >= "3.10.0") run <- TRUE
if (run) {
JTSplot <- function(x, fill="grey90", pts="black", main="", xlim, ylim) {
  if (inherits(x, "SpatialCollections")) {
    xl <- xp <- xpl <- NULL
    if (!is.null(slot(x, "lineobj"))) xl <- slot(x, "lineobj")
    if (!is.null(slot(x, "pointobj"))) xp <- slot(x, "pointobj")
    if (!is.null(slot(x, "polyobj"))) xpl <- slot(x, "polyobj")
    if (is.null(xl)) xl <- as(xpl, "SpatialLines")
    else xl <- rbind(xl, as(xpl, "SpatialLines"))
    if (is.null(xp)) xp <- as(xl, "SpatialPoints")
    else xp <- rbind(xp, as(xl, "SpatialPoints"))
  } else {
    xl <- as(x, "SpatialLines")
    xp <- as(xl, "SpatialPoints")
  }
  plot(coordinates(xp), type="n", main=main, xlab="", ylab="", axes=FALSE, xlim=xlim, ylim=ylim)
  plot(x, col=fill, border="transparent", add=TRUE, xlim=xlim, ylim=ylim)
  plot(xl, col=pts, add=TRUE, xlim=xlim, ylim=ylim)
  plot(xp, col=pts, add=TRUE, pch=14, xlim=xlim, ylim=ylim)
}
}
if (run) {
X <- readWKT(paste0("POLYGON ((10 70, 90 70, 90 50, 30 50, 30 30, ",
 "50 30, 50 90, 70 90, 70 10, 10 10, 10 70))", sep=""))
bb <- bbox(X)
opar <- par(mfrow=c(2,2))
JTSplot(X, fill="#dfdfff", pts="#00007c", main="input", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=TRUE), fill="#ffffa4", pts="#78b400",
 main="linework", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=FALSE), fill="#ffffa4", pts="#78b400",
 main="structure", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=FALSE, keepCollapsed=TRUE), fill="#ffffa4",
 pts="#78b400", main="structure + keep collapsed", xlim=bb[1,], ylim=bb[2,])
par(opar)
}
if (run) {
X <- readWKT(paste0("POLYGON ((10 70, 90 70, 90 10, 10 10, 10 70), ",
 "(60 80, 50 30, 0 20, 10 30, 60 80), (30 40, 60 30, 40 0, 30 40))", sep=""))
bb <- rbind(c(0, 90), c(0, 80))
opar <- par(mfrow=c(2,2))
JTSplot(X, fill="#dfdfff", pts="#00007c", main="input", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=TRUE), fill="#ffffa4", pts="#78b400",
 main="linework", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=FALSE), fill="#ffffa4", pts="#78b400",
 main="structure", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=FALSE, keepCollapsed=TRUE), fill="#ffffa4",
 pts="#78b400", main="structure + keep collapsed", xlim=bb[1,], ylim=bb[2,])
par(opar)
}
if (run) {
X <- readWKT(paste0("MULTIPOLYGON (((10 90, 60 90, 60 40, 10 40, 10 90)), ",
 "((40 60, 70 60, 70 30, 40 30, 40 60)), ((20 50, 50 50, 50 20, 20 20, 20 50)))", sep=""))
bb <- bbox(X)
opar <- par(mfrow=c(2,2))
JTSplot(X, fill="#dfdfff", pts="#00007c", main="input", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=TRUE), fill="#ffffa4", pts="#78b400",
 main="linework", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=FALSE), fill="#ffffa4", pts="#78b400",
 main="structure", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=FALSE, keepCollapsed=TRUE), fill="#ffffa4",
 pts="#78b400", main="structure + keep collapsed", xlim=bb[1,], ylim=bb[2,])
par(opar)
}
if (run) {
X <- readWKT(paste0("MULTIPOLYGON (((10 40, 40 40, 40 10, 10 10, 10 40), ", 
"(16 17, 31 32, 24 25, 16 17)), ((50 40, 50 40, 50 40, 50 40, 50 40)))", sep=""))
bb <- bbox(X)
opar <- par(mfrow=c(2,2))
JTSplot(X, fill="#dfdfff", pts="#00007c", main="input", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=TRUE), fill="#ffffa4", pts="#78b400",
 main="linework", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=FALSE), fill="#ffffa4", pts="#78b400",
 main="structure", xlim=bb[1,], ylim=bb[2,])
JTSplot(gMakeValid(X, original=FALSE, keepCollapsed=TRUE), fill="#ffffa4",
 pts="#78b400", main="structure + keep collapsed", xlim=bb[1,], ylim=bb[2,])
par(opar)
}
if (run) {
X <- readWKT(paste0("MULTIPOLYGON (((10 40, 40 40, 40 10, 10 10, 10 40), ", 
"(16 17, 31 32, 24 25, 16 17)), ((50 40, 50 40, 50 40, 50 40, 50 40)))", sep=""))
bb <- bbox(X)
opar <- par(mfrow=c(2,2))
JTSplot(X, fill="#dfdfff", pts="#00007c", main="input", xlim=bb[1,], ylim=bb[2,])
Sys.setenv("GEOS_MAKE_VALID"="LINEWORK")
JTSplot(gMakeValid(X), fill="#ffffa4", pts="#78b400", main="linework (env-var)",
 xlim=bb[1,], ylim=bb[2,])
Sys.setenv("GEOS_MAKE_VALID"="STRUCTURE")
Sys.setenv("GEOS_MAKE_VALID_KEEPCOLLAPSED"="FALSE")
JTSplot(gMakeValid(X), fill="#ffffa4", pts="#78b400", main="structure (env-vars)",
 xlim=bb[1,], ylim=bb[2,])
Sys.setenv("GEOS_MAKE_VALID_KEEPCOLLAPSED"="TRUE")
JTSplot(gMakeValid(X), fill="#ffffa4", pts="#78b400", main="structure + keep collapsed (env-vars)",
 xlim=bb[1,], ylim=bb[2,])
par(opar)
Sys.unsetenv("GEOS_MAKE_VALID")
Sys.unsetenv("GEOS_MAKE_VALID_KEEPCOLLAPSED")

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

