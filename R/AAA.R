.RGEOS_HANDLE <- new.env(FALSE, parent=globalenv())

set_RGEOS_HANDLE <- function(handle) {
    assign("GEOSptr", handle, envir=.RGEOS_HANDLE)
}

set_RGEOS_DENSE <- function(value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1)
    assign("returnDense", value, envir=.RGEOS_HANDLE)
}

get_RGEOS_DENSE <- function() {
    get("returnDense", envir=.RGEOS_HANDLE)
}

set_RGEOS_dropSlivers <- function(value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1)
    assign("dropSlivers", value, envir=.RGEOS_HANDLE)
}

get_RGEOS_dropSlivers <- function() {
    get("dropSlivers", envir=.RGEOS_HANDLE)
}

set_RGEOS_warnSlivers <- function(value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1)
    assign("warnSlivers", value, envir=.RGEOS_HANDLE)
}

get_RGEOS_warnSlivers <- function() {
    get("warnSlivers", envir=.RGEOS_HANDLE)
}

set_RGEOS_polyThreshold <- function(value) {
    stopifnot(is.numeric(value))
    stopifnot(length(value) == 1)
    stopifnot(value >= 0.0)
    assign("polyThreshold", value, envir=.RGEOS_HANDLE)
}

get_RGEOS_polyThreshold <- function() {
    get("polyThreshold", envir=.RGEOS_HANDLE)
}

set_RGEOS_CheckValidity <- function(value) {
    stopifnot(is.integer(value))
    stopifnot(length(value) == 1)
    stopifnot(value >= 0L && value < 3L)
    assign("CheckValidity", value, envir=.RGEOS_HANDLE)
}

get_RGEOS_CheckValidity <- function() {
    get("CheckValidity", envir=.RGEOS_HANDLE)
}

set_RGEOS_STR <- function(value) {
    stopifnot(is.logical(value))
    stopifnot(length(value) == 1)
    assign("STRsubset", value, envir=.RGEOS_HANDLE)
}

get_RGEOS_STR <- function() {
    get("STRsubset", envir=.RGEOS_HANDLE)
}



init_RGEOS <- function() {
    .Call('rgeos_Init', PACKAGE="rgeos")
}

finish_RGEOS <- function() {
    .Call('rgeos_finish', .RGEOS_HANDLE, PACKAGE="rgeos")
}

version_GEOS <- function(runtime=TRUE) {
    stopifnot(is.logical(runtime))
    stopifnot(length(runtime) == 1L)
    res0 <- .Call("rgeos_GEOSversion", runtime, PACKAGE="rgeos")
    res1 <- strsplit(res0, " ")[[1]]
    res <- res1[1]
    if (length(res1) > 1) attr(res, "rev") <- res1[2]
    res
}

version_GEOS0 <- function() {
#    substring(version_GEOS(), 1, 5)
    package_version(gsub("[a-zA-Z]", "", strsplit(version_GEOS(), "-")[[1]][1]))
}

version_sp_linkingTo <- function() {
    .Call("rgeos_sp_linkingTo_version")
}

load_stuff <- function() {
  set_RGEOS_HANDLE(init_RGEOS())
  if (!isTRUE(all.equal(version_GEOS(TRUE), version_GEOS(FALSE),
    check.attributes=FALSE))) {
    warning("rgeos: versions of GEOS runtime ", c(version_GEOS(TRUE)),
    "\nand GEOS at installation ", version_GEOS(FALSE), "differ")
  }
  assign("scale", 100000000, envir=.RGEOS_HANDLE)
  assign("do_poly_check", TRUE, envir=.RGEOS_HANDLE)
#  assign("both_poly", FALSE, envir=.RGEOS_HANDLE)
#  assign("drop_not_poly", FALSE, envir=.RGEOS_HANDLE)
  assign("polyThreshold", 0.0, envir=.RGEOS_HANDLE)
  assign("dropSlivers", FALSE, envir=.RGEOS_HANDLE)
  assign("warnSlivers", TRUE, envir=.RGEOS_HANDLE)
  assign("returnDense", TRUE, envir=.RGEOS_HANDLE)
  assign("STRsubset", FALSE, envir=.RGEOS_HANDLE)
  cV <- ifelse(c(version_GEOS0()) < "3.7.2", 0L, 1L)
  assign("CheckValidity", cV, envir=.RGEOS_HANDLE)
  fn <- system.file("SVN_VERSION", package="rgeos")
  if (file.exists(fn)) {
    svn_version <- scan(fn, what=character(1), sep="\n", quiet=TRUE)
  } else {
    svn_version <- "(unknown)"
  }
  Smess <- paste("rgeos version: ", utils::packageDescription("rgeos")$Version,
    ", (SVN revision ", svn_version, ")\n", sep="")
  Smess <- paste(Smess, "GEOS runtime version:",
    version_GEOS(), "\n")
  Smess <- paste(Smess, "Please note that rgeos will be retired during October 2023,\nplan transition to sf or terra functions using GEOS at your earliest convenience.\nSee https://r-spatial.org/r/2023/05/15/evolution4.html for details.\n")
  if (gIsOverlayNG()) Smess <- paste(Smess, "GEOS using OverlayNG\n")
  splVersion <- version_sp_linkingTo()
  Smess <- paste(Smess, "Linking to sp version:", splVersion, "\n")
  spVcheck <- NULL
  if("sp" %in% .packages()) spVcheck <- utils::packageVersion("sp") == splVersion
  if (!is.null(spVcheck) && !spVcheck) paste(Smess, 
    "sp version used to install rgeos and loaded sp version differ\n")

  Smess <- paste(Smess, "Polygon checking:", get_do_poly_check(), "\n")
  packageStartupMessage(Smess, appendLF = TRUE)

}

.onLoad <- function(lib, pkg) {
  load_stuff()
}

.onAttach <- function(lib, pkg) {
}

.onUnload <- function(libpath) {
  invisible(finish_RGEOS())
}

rgeos_extSoftVersion <- function() {
  res <- c("GEOS"=strsplit(version_GEOS(), "-")[[1]][1], "OverlayNG"=gIsOverlayNG(), "sp"=version_sp_linkingTo())
  res
}
