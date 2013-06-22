RGEOSBinPredFunc = function(spgeom1, spgeom2, byid, func, optparam=NULL) {
    stopifnot(is.logical(byid))
    byid = as.logical(byid)
    if (any(is.na(byid)) ) stop("Invalid value for byid, must be logical")

    if( length(byid) < 1 || length(byid) > 2 )
        stop("Invalid length for byid, must be of length 1 or 2")

    if (length(byid) == 1)
        byid <- rep(byid,2)

    if (is.null(spgeom2) && inherits(spgeom1, "SpatialPolygons") 
        && get_do_poly_check() && notAllComments(spgeom1)) 
        spgeom1 <- createSPComment(spgeom1)

    if(!is.null(spgeom1) & !is.null(spgeom2)) {
        if(!identical(spgeom1@proj4string,spgeom2@proj4string))
            warning("spgeom1 and spgeom2 have different proj4 strings")
        if (inherits(spgeom1, "SpatialPolygons") && get_do_poly_check() && notAllComments(spgeom1)) 
            spgeom1 <- createSPComment(spgeom1)
        if (inherits(spgeom2, "SpatialPolygons") && get_do_poly_check() && notAllComments(spgeom2)) 
            spgeom2 <- createSPComment(spgeom2)
    }
	
    if ( func == "rgeos_equalsexact" )         
        x <- .Call("rgeos_equalsexact", .RGEOS_HANDLE, spgeom1, spgeom2, optparam, byid, PACKAGE="rgeos")
    else if (func == "rgeos_relatepattern")
        x <- .Call("rgeos_relatepattern", .RGEOS_HANDLE, spgeom1, spgeom2, optparam, byid, PACKAGE="rgeos")
    else if (func == "rgeos_contains")
        x <- .Call("rgeos_contains", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_contains_prepared")
        x <- .Call("rgeos_contains_prepared", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_intersects")
        x <- .Call("rgeos_intersects", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_intersects_prepared")
        x <- .Call("rgeos_intersects_prepared", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_containsproperly_prepared")
        x <- .Call("rgeos_containsproperly_prepared", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_covers_prepared")
        x <- .Call("rgeos_covers_prepared", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_disjoint")
        x <- .Call("rgeos_disjoint", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_touches")
        x <- .Call("rgeos_touches", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_crosses")
        x <- .Call("rgeos_crosses", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_within")
        x <- .Call("rgeos_within", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_overlaps")
        x <- .Call("rgeos_overlaps", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_equals")
        x <- .Call("rgeos_equals", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else if (func == "rgeos_relate")
        x <- .Call("rgeos_relate", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    else stop("No such function:", func)    
    if(any(byid)) {
        id1 = row.names(spgeom1) 
        if (is.null(spgeom2)) id2 = id1
        else id2 = row.names(spgeom2)

        colnames(x) <- id1
        rownames(x) <- id2
    }
    
    return(x)
}



gContains = function(spgeom1, spgeom2 = NULL, byid = FALSE, prepared=TRUE) {
    func = "rgeos_contains"
	if (prepared)
		func = paste(func,"_prepared",sep="")

	return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,func) )
}
gIntersects = function(spgeom1, spgeom2 = NULL, byid = FALSE,prepared=TRUE) {
	func = "rgeos_intersects"
	if (prepared)
		func = paste(func,"_prepared",sep="")

	return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,func) )
}

gContainsProperly = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_containsproperly_prepared") )
}
gCovers = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_covers_prepared") )
}
gCoveredBy = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom2,spgeom1,rev(byid),"rgeos_covers_prepared") )
}


gDisjoint = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_disjoint") )
}
gTouches = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_touches") )
}
gCrosses = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_crosses") )
}
gWithin = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_within") )
}
gOverlaps = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_overlaps") )
}
gEquals = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_equals") )
}
gEqualsExact = function(spgeom1, spgeom2 = NULL, tol=0.0, byid = FALSE) {
    tol <- as.numeric(tol)
    if ( is.na(tol) ) 
        stop("Invalid value for tolerance, must be numeric")

    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_equalsexact", tol) )
}

gRelate = function(spgeom1, spgeom2 = NULL, pattern = NULL, byid = FALSE) {
    
	if (is.null(pattern)) {
		return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_relate") )
	} else {
		if ( !is.character(pattern) )
			stop("Invalid value for pattern, must be character")
		pattern = toupper(pattern)
		
	    if (length(pattern) != 1)
	        stop("Pattern must have length of 1")
    
	    if (nchar(pattern) != 9 || !grepl("[0-2TF\\*]{9}",pattern) )
	        stop("Invalid pattern, see documentation for proper format")
    
		return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_relatepattern",pattern) )
	}
}





RGEOSDisjoint = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gDisjoint")
    return( gDisjoint(spgeom1,spgeom2, byid) )
}
RGEOSTouches = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gTouches")
    return( gTouches(spgeom1,spgeom2, byid) )
}
RGEOSIntersects = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gIntersects")
    return( gIntersects(spgeom1,spgeom2, byid) )
}
RGEOSCrosses = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gCrosses")
    return( gCrosses(spgeom1,spgeom2, byid) )
}
RGEOSWithin = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gWithin")
    return( gWithin(spgeom1,spgeom2, byid) )
}
RGEOSContains = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gContains")
    return( gContains(spgeom1,spgeom2, byid) )
}
RGEOSOverlaps = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gOverlaps")
    return( gOverlaps(spgeom1,spgeom2, byid) )
}
RGEOSEquals = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gEquals")
    return( gEquals(spgeom1,spgeom2, byid) )
}
RGEOSEqualsExact = function(spgeom1, spgeom2 = NULL, tol=0.0, byid = FALSE) {
    .Deprecated("gEqualsExact")
    return( gEqualsExact(spgeom1, spgeom2, byid, tol) )
}
RGEOSRelate = function(spgeom1, spgeom2 = NULL, pattern = NULL, byid = FALSE) {
    .Deprecated("gRelate")
    return( gRelate(spgeom1,spgeom2, byid, pattern) )
}
