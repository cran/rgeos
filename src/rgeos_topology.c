#include "rgeos.h"


SEXP rgeos_envelope(SEXP env, SEXP obj, SEXP id, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSEnvelope_r) );
}

SEXP rgeos_convexhull(SEXP env, SEXP obj, SEXP id, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSConvexHull_r) );
}

SEXP rgeos_boundary(SEXP env, SEXP obj, SEXP id, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSBoundary_r) );
}
    
SEXP rgeos_getcentroid(SEXP env, SEXP obj, SEXP id, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSGetCentroid_r) );
}

SEXP rgeos_pointonsurface(SEXP env, SEXP obj, SEXP id, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSPointOnSurface_r) );
}

SEXP rgeos_linemerge(SEXP env, SEXP obj, SEXP id, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSLineMerge_r) );
}

SEXP rgeos_unioncascaded(SEXP env, SEXP obj, SEXP id, SEXP byid ) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSUnionCascaded_r) );
}

#ifdef HAVEUNARYUNION

SEXP rgeos_unaryunion(SEXP env, SEXP obj, SEXP id, SEXP byid ) {

    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSUnaryUnion_r) ); 
}

#endif

SEXP rgeos_topologyfunc(SEXP env, SEXP obj, SEXP id, SEXP byid, 
                        GEOSGeom (*topofunc)(GEOSContextHandle_t, const GEOSGeom) ) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    SEXP p4s = GET_SLOT(obj, install("proj4string"));
    GEOSGeom geom = rgeos_convert_R2geos(env, obj);
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    int n = 1;
    if (LOGICAL_POINTER(byid)[0] && type == GEOS_GEOMETRYCOLLECTION)
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    
    if (n < 1) error("rgeos_topologyfunc: invalid number of geometries");
    
    GEOSGeom *resgeoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
    
    GEOSGeom curgeom = geom;
    int curtype = GEOSGeomTypeId_r(GEOShandle, curgeom);
    for(int i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_topologyfunc: unable to get subgeometries");
            curtype = GEOSGeomTypeId_r(GEOShandle, curgeom);
        }
        if (topofunc == GEOSUnionCascaded_r && curtype == GEOS_POLYGON) {
            resgeoms[i] = curgeom;
        } else {
            resgeoms[i] = topofunc(GEOShandle, curgeom);
            if (resgeoms[i] == NULL)
                error("rgeos_topologyfunc: unable to calculate");
        }


    }
    
    GEOSGeom res = resgeoms[0];
    if (n > 1)
        res = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, resgeoms, n);
    
    //FIXME - crashes if geom created from a collection created by R_Alloc
    //GEOSGeom_destroy_r(GEOShandle, geom);
        
    return( rgeos_convert_geos2R(env, res, p4s, id) );
}


SEXP rgeos_simplify(SEXP env, SEXP obj, SEXP tol, SEXP id, SEXP byid, SEXP topPres) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    SEXP p4s = GET_SLOT(obj, install("proj4string"));
    GEOSGeom geom = rgeos_convert_R2geos(env, obj);
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    
	int preserve = LOGICAL_POINTER(topPres)[0];
	double tolerance = NUMERIC_POINTER(tol)[0];

    int n = 1;
    if (LOGICAL_POINTER(byid)[0] && type == GEOS_GEOMETRYCOLLECTION)
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    
    if (n < 1) error("rgeos_simplify: invalid number of geometries");
    
    GEOSGeom *resgeoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
    
    GEOSGeom curgeom = geom;
    int curtype = GEOSGeomTypeId_r(GEOShandle, curgeom);
    for(int i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_topologyfunc: unable to get subgeometries");
            curtype = GEOSGeomTypeId_r(GEOShandle, curgeom);
        }
        
		if (preserve) {
			resgeoms[i] = GEOSTopologyPreserveSimplify_r(GEOShandle, curgeom, tolerance);
		} else {
			resgeoms[i] = GEOSSimplify_r(GEOShandle, curgeom, tolerance);
		}
    }
    
    GEOSGeom res = (n == 1) ? resgeoms[0] :
    				GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, resgeoms, n);
  
    return( rgeos_convert_geos2R(env, res, p4s, id) );
}

SEXP rgeos_polygonize(SEXP env, SEXP obj, SEXP id, SEXP p4s, SEXP cutEdges) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

	int getCutEdges = LOGICAL_POINTER(cutEdges)[0];
    int n = length(obj);
    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
    
    for(int i=0; i<n; i++) {    
		geoms[i] = rgeos_convert_R2geos(env, VECTOR_ELT(obj,i));
	}
	
	GEOSGeom res = (getCutEdges) ? 
					GEOSPolygonizer_getCutEdges_r(GEOShandle, geoms, (unsigned int) n) :
					GEOSPolygonize_r(GEOShandle, geoms, (unsigned int) n);
	
    return( rgeos_convert_geos2R(env, res, p4s, id) );
}
