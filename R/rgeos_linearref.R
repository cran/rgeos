#' Project Points to Line Geometry
#'
#' Return distances along geometry to points nearest the specified points.
#'
#' @param spgeom SpatialLines or SpatialLinesDataFrame object
#' @param sppoint SpatialPoints or SpatialPointsDataFrame object
#' @param normalized Logical determining if normalized distances
#'   should be used
#' @return a numeric vector containing the distances along the line to
#'   points nearest to the specified points
#' @details If \code{normalized=TRUE}, distances normalized to the length
#'   of the geometry are returned, i.e., values between 0 and 1.
#' @seealso gInterpolate
#' @rdname linref-gProject
#' @author Rainer Stuetz
#' @keywords spatial
#' @examples
#' l <- readWKT("LINESTRING(0 1, 3 4, 5 6)")
#' p1 <- readWKT("MULTIPOINT(3 2, 3 5)")
#' frac <- gProject(l, p1)
#' p2 <- gInterpolate(l, frac)
#' plot(l, axes=TRUE)
#' plot(p1, col = "blue", add = TRUE)
#' plot(p2, col = "red", add = TRUE)
#' @export
gProject <- function(spgeom, sppoint, normalized = FALSE) {

  stopifnot(inherits(spgeom, "SpatialLines") ||
            inherits(spgeom, "SpatialLinesDataFrame"))
  stopifnot(inherits(sppoint, "SpatialPoints") ||
            inherits(sppoint, "SpatialPointsDataFrame"))
  stopifnot(is.finite(normalized))

  x <- .Call("rgeos_project", .RGEOS_HANDLE, spgeom, sppoint, normalized)
  x
}


#' Interpolate Points along Line Geometry
#'
#' Return points at specified distances along a line.
#'
#' @param spgeom SpatialLines or SpatialLinesDataFrame object
#' @param d Numeric vector specifying the distance along the line geometry
#' @param normalized Logical determining if normalized distances
#'   should be used
#' @return SpatialPoints object
#' @details If \code{normalized=TRUE}, the distances will be interpreted
#'   as fractions of the line length.
#' @seealso gInterpolate
#' @rdname linref-gInterpolate
#' @author Rainer Stuetz
#' @keywords spatial
#' @examples
#' gInterpolate(readWKT("LINESTRING(25 50, 100 125, 150 190)"),
#'              d=seq(0, 1, by = 0.2), normalized = TRUE)
#' @export
gInterpolate <- function(spgeom, d, normalized = FALSE) {

  stopifnot(inherits(spgeom, "SpatialLines") ||
            inherits(spgeom, "SpatialLinesDataFrame"))
  stopifnot(all(is.finite(d)))
  stopifnot(is.finite(normalized))

  x <- .Call("rgeos_interpolate", .RGEOS_HANDLE, spgeom, d, normalized)
  rownames(x) <- seq_len(nrow(x))
  SpatialPoints(x, proj4string=CRS(proj4string(spgeom)))
}
