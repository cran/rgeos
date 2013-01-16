.RGEOS_HANDLE <- new.env(FALSE, parent=globalenv())

set_RGEOS_HANDLE <- function(handle) {
    assign("GEOSptr", handle, envir=.RGEOS_HANDLE)
}

init_RGEOS <- function() {
    .Call('rgeos_Init', PACKAGE="rgeos")
}

finish_RGEOS <- function() {
    .Call('rgeos_finish', .RGEOS_HANDLE, PACKAGE="rgeos")
}

version_GEOS <- function() {
    .Call("rgeos_GEOSversion", PACKAGE="rgeos")
}

version_GEOS0 <- function() {
    substring(version_GEOS(), 1, 5)
}

.onLoad <- function(lib, pkg) {
#  require(methods, quietly = TRUE, warn.conflicts = FALSE)
#  require("sp")
#  require("stringr")
#  library.dynam('rgeos', pkg, lib)

  set_RGEOS_HANDLE(init_RGEOS())
  assign("scale", 100000000, envir=.RGEOS_HANDLE)
  assign("do_poly_check", TRUE, envir=.RGEOS_HANDLE)

}

.onAttach <- function(lib, pkg) {
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
  Smess <- paste(Smess, "Polygon checking:", get_do_poly_check(), "\n")
  packageStartupMessage(Smess, appendLF = TRUE)
}

.onUnload <- function(libpath) {
  invisible(finish_RGEOS())
}
