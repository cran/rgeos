

gBuffer = function(spgeom, byid=FALSE, id=NULL, width=1.0, quadsegs=5, 
                     capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0) {

    stopifnot(is.logical(byid))
    if (!is.na(is.projected(spgeom)) && !is.projected(spgeom))
     warning("Spatial object is not projected; GEOS expects planar coordinates")

    GEOSCapStyles = c("ROUND","FLAT","SQUARE")
    GEOSJoinStyles = c("ROUND","MITRE","BEVEL")

    
    curids = unique(row.names(spgeom))
    if (is.null(id)) {
        if (byid)   id = curids
        else        id = "buffer"
    }
    
    if (byid == TRUE  && length(id) != length(curids) )
        stop("if applying by ids, new ids must be same length as current ids")
    if (byid == FALSE && length(id) != 1 ) 
        stop("if applying across ids, new id must be length 1")
    
    id = as.character(id)
    width = as.numeric(width)
    quadsegs = as.integer(quadsegs)
    byid = as.logical(byid)
    mitreLimit=as.numeric(mitreLimit)
    
    if (is.character(capStyle)) 
        capStyle = which(match.arg(toupper(capStyle),GEOSCapStyles) == GEOSCapStyles)
    if (is.character(joinStyle)) 
        joinStyle = which(match.arg(toupper(joinStyle),GEOSJoinStyles) == GEOSJoinStyles)
    
    if ( !(capStyle %in% 1:length(GEOSCapStyles)) ) stop("invalid cap style")
    if ( !(joinStyle %in% 1:length(GEOSJoinStyles)) ) stop("invalid join style")
    
    capStyle= as.integer(capStyle)
    joinStyle= as.integer(joinStyle)
    
    if (mitreLimit <= 0) 
        stop("mitreLimit must be greater than 0")
    if (capStyle == 2 && inherits(spgeom,"SpatialPoints")) 
        stop("Flat capstyle is incompatible with SpatialPoints geometries")
    if (width < 0 && !inherits(spgeom,"SpatialPolygons")) 
        stop("Negative width values may only be used with SpatialPolygons geometries")

    ans = .Call("rgeos_buffer", .RGEOS_HANDLE, spgeom, byid, id, width, quadsegs,
                                capStyle, joinStyle, mitreLimit, PACKAGE="rgeos")
 
	if (byid) {
		if (.hasSlot(spgeom, 'data')) {
			ans <- SpatialPolygonsDataFrame(ans, spgeom@data, FALSE)
		}	
	}
 
    return(ans)
}

RGEOSBuffer = function(spgeom, byid=TRUE, id=NULL, width=1.0, quadsegs=5, 
                       capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0) {
   .Deprecated("gBuffer")
   return( gBuffer(spgeom, byid, id, width, quadsegs, capStyle, joinStyle, mitreLimit) )
}

#RGEOSSingleSidedBuffer = function(spgeom, width, quadsegs, joinStyle, mitreLimit) {
#    
#    GEOSCapStyles = c("ROUND","FLAT","SQUARE")
#    GEOSJoinStyles = c("ROUND","MITRE","BEVEL")
#}
