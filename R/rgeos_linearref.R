gProject <- function(spgeom, sppoint, normalized = FALSE) {

  stopifnot(inherits(spgeom, "SpatialLines") ||
            inherits(spgeom, "SpatialLinesDataFrame"))
  stopifnot(inherits(sppoint, "SpatialPoints") ||
            inherits(sppoint, "SpatialPointsDataFrame"))
  stopifnot(is.finite(normalized))

  x <- .Call("rgeos_project", .RGEOS_HANDLE, spgeom, sppoint, normalized)
  x
}


gInterpolate <- function(spgeom, d, normalized = FALSE) {

  stopifnot(inherits(spgeom, "SpatialLines") ||
            inherits(spgeom, "SpatialLinesDataFrame"))
  stopifnot(all(is.finite(d)))
  stopifnot(is.finite(normalized))

  x <- .Call("rgeos_interpolate", .RGEOS_HANDLE, spgeom, d, normalized)
  rownames(x) <- seq_len(nrow(x))
  SpatialPoints(x, proj4string=CRS(proj4string(spgeom)))
}
