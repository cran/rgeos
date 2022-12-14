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

#ifdef HAVE_UNARYUNION
SEXP rgeos_unaryunion(SEXP env, SEXP obj, SEXP id, SEXP byid ) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSUnaryUnion_r) ); 
}
#endif

#ifdef HAVE_MINIMUMROTATEDRECTANGLE
SEXP rgeos_minimumrotatedrectangle(SEXP env, SEXP obj, SEXP id, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSMinimumRotatedRectangle_r) );
}
#endif

#ifdef HAVE_MAKEVALID
SEXP rgeos_makevalid(SEXP env, SEXP obj, SEXP id, SEXP byid ) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSMakeValid_r) ); 
}
#endif

#ifdef HAVE_MAKEVALIDPARAMS
SEXP rgeos_makevalidparams(SEXP env, SEXP obj, SEXP id, SEXP byid ) {

    SEXP original, keepCollapsed;
    original = getAttrib(byid, install("original"));
    keepCollapsed = getAttrib(byid, install("keepCollapsed"));

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    SEXP p4s = GET_SLOT(obj, install("proj4string"));
    GEOSGeom geom = rgeos_convert_R2geos(env, obj);
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    int n = 1;
    if (LOGICAL_POINTER(byid)[0] && type == GEOS_GEOMETRYCOLLECTION)
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    
    if (n < 1) error("rgeos_makevalidparams: invalid number of geometries");

    GEOSMakeValidParams *p = GEOSMakeValidParams_create_r(GEOShandle);

    if (LOGICAL_POINTER(original)[0]) {
        GEOSMakeValidParams_setMethod_r(GEOShandle, p,
            GEOS_MAKE_VALID_LINEWORK);
    } else {
        GEOSMakeValidParams_setMethod_r(GEOShandle, p,
            GEOS_MAKE_VALID_STRUCTURE);
    }
    GEOSMakeValidParams_setKeepCollapsed_r(GEOShandle, p,
        (int) LOGICAL_POINTER(keepCollapsed)[0]);
    
    GEOSGeom *resgeoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
    
    for(int i=0; i<n; i++) {
        const GEOSGeometry *curgeom = (n > 1) ? 
            (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i) : geom;
        
        if (curgeom == NULL) 
            error("rgeos_makevalidparams: unable to get subgeometries");

        resgeoms[i] = GEOSMakeValidWithParams_r(GEOShandle, curgeom, p);
        
        if (resgeoms[i] == NULL) {
            GEOSGeom_destroy_r(GEOShandle, geom);
            GEOSMakeValidParams_destroy_r(GEOShandle, p);
            char* errbuf = get_errbuf();
            error(errbuf);
        }
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom);
    GEOSMakeValidParams_destroy_r(GEOShandle, p);
    
    GEOSGeom res = (n == 1) ? resgeoms[0]
                    : GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, resgeoms, (unsigned int) n);
    
    return( rgeos_convert_geos2R(env, res, p4s, id) ); // releases res
}
#endif

#ifdef HAVE_MAXIMUMINSSCRIBEDCIRCLE
SEXP rgeos_maximuminscribedcircle(SEXP env, SEXP obj, SEXP id, SEXP byid, SEXP tol) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    SEXP p4s = GET_SLOT(obj, install("proj4string"));
    GEOSGeom geom = rgeos_convert_R2geos(env, obj);
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    int n = 1;
    if (LOGICAL_POINTER(byid)[0] && type == GEOS_GEOMETRYCOLLECTION)
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    
    if (n < 1) error("rgeos_maximuminscribedcircle: invalid number of geometries");
    
    GEOSGeom *resgeoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
    
    for(int i=0; i<n; i++) {
        const GEOSGeometry *curgeom = (n > 1) ? (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i)
                                       : geom;
        
        if (curgeom == NULL) 
            error("rgeos_maximuminscribedcircle: unable to get subgeometries");
        
        resgeoms[i] = GEOSMaximumInscribedCircle_r(GEOShandle, curgeom,
            NUMERIC_POINTER(tol)[0]);
        if (resgeoms[i] == NULL) {
            GEOSGeom_destroy_r(GEOShandle, geom);
            char* errbuf = get_errbuf();
            error(errbuf);
        }
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom);
    
    GEOSGeom res = (n == 1) ? resgeoms[0]
                    : GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, resgeoms, (unsigned int) n);
    
    return( rgeos_convert_geos2R(env, res, p4s, id) ); // releases res
}
#endif


#ifdef HAVE_DELAUNAY
SEXP rgeos_delaunaytriangulation(SEXP env, SEXP obj, SEXP tol,
  SEXP onlyEdges) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    double tolerance = NUMERIC_POINTER(tol)[0];
    int oE = INTEGER_POINTER(onlyEdges)[0];
    int pc=0;

    SEXP ans, id;
    
    SEXP p4s = GET_SLOT(obj, install("proj4string"));

    GEOSGeom geom = rgeos_convert_R2geos(env, obj);

    GEOSGeom resgeom = GEOSDelaunayTriangulation_r(GEOShandle, geom,
        tolerance, oE);
    if (resgeom == NULL)
            error("rgeos_delaunaytriangulation: unable to compute");

    GEOSGeom_destroy_r(GEOShandle, geom);

//    int type = GEOSGeomTypeId_r(GEOShandle, resgeom);

    int ng = GEOSGetNumGeometries_r(GEOShandle, resgeom);

//Rprintf("ng: %d, type: %d, %s\n", ng, type, GEOSGeomType_r(GEOShandle, resgeom));
// FIXME convert type 5 to type 7

    char buf[BUFSIZ];

    PROTECT(id = NEW_CHARACTER(ng)); pc++;
    for (int i=0; i<ng; i++) {
        snprintf(buf, sizeof(buf), "%d", i);
        SET_STRING_ELT(id, i, COPY_TO_USER_STRING(buf));
    }

    ans = rgeos_convert_geos2R(env, resgeom, p4s, id); 

    UNPROTECT(pc);
    return(ans);

}
#endif


#ifdef HAVE_COVERAGEUNION
SEXP rgeos_coverageunion(SEXP env, SEXP obj, SEXP id, SEXP byid ) {
    return( rgeos_topologyfunc(env, obj, id, byid, &GEOSCoverageUnion_r) ); 
}
#endif



SEXP rgeos_topologyfunc(SEXP env, SEXP obj, SEXP id, SEXP byid, p_topofunc topofunc) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    SEXP p4s = GET_SLOT(obj, install("proj4string"));
    GEOSGeom geom = rgeos_convert_R2geos(env, obj);
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    int n = 1;
    if (LOGICAL_POINTER(byid)[0] && type == GEOS_GEOMETRYCOLLECTION)
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    
    if (n < 1) error("rgeos_topologyfunc: invalid number of geometries");
    
    GEOSGeom *resgeoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
    
    for(int i=0; i<n; i++) {
        const GEOSGeometry *curgeom = (n > 1) ? (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i)
                                       : geom;
        
        if (curgeom == NULL) 
            error("rgeos_topologyfunc: unable to get subgeometries");
        
        if (    topofunc == GEOSUnionCascaded_r
             && GEOSGeomTypeId_r(GEOShandle, curgeom) == GEOS_POLYGON) {
            resgeoms[i] = GEOSGeom_clone_r(GEOShandle, curgeom);
        } else {
            resgeoms[i] = topofunc(GEOShandle, curgeom);
            if (resgeoms[i] == NULL) {
                GEOSGeom_destroy_r(GEOShandle, geom);
                char* errbuf = get_errbuf();
                error(errbuf);
            }
//                error("rgeos_topologyfunc: unable to calculate");
        }
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom);
    
    GEOSGeom res = (n == 1) ? resgeoms[0]
                    : GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, resgeoms, (unsigned int) n);
    
    return( rgeos_convert_geos2R(env, res, p4s, id) ); // releases res
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
    
    for(int i=0; i<n; i++) {
        const GEOSGeometry *curgeom = (n > 1) ? (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i)
                                       : geom;
        if (curgeom == NULL)
            error("rgeos_topologyfunc: unable to get subgeometries");
        
        resgeoms[i] = (preserve)
                        ? GEOSTopologyPreserveSimplify_r(GEOShandle, curgeom, tolerance)
                        : GEOSSimplify_r(GEOShandle, curgeom, tolerance);
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom);
    
    GEOSGeom res = (n == 1) ? resgeoms[0] :
                    GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, resgeoms, (unsigned int) n);
    
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
    
    GEOSGeom res = (getCutEdges)
                     ? GEOSPolygonizer_getCutEdges_r(GEOShandle, (const GEOSGeometry *const *) geoms, (unsigned int) n)
                     : GEOSPolygonize_r(GEOShandle, (const GEOSGeometry *const *) geoms, (unsigned int) n);
    
    return( rgeos_convert_geos2R(env, res, p4s, id) );
}

#ifdef HAVE_NODE
SEXP rgeos_node(SEXP env, SEXP obj) {

    SEXP ans, id;
    int pc=0;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    SEXP p4s = GET_SLOT(obj, install("proj4string"));
    GEOSGeom geom = rgeos_convert_R2geos(env, obj);
//    int type = GEOSGeomTypeId_r(GEOShandle, geom);
//Rprintf("type: %d, %s\n", type, GEOSGeomType_r(GEOShandle, geom));
    
    GEOSGeom res = GEOSNode_r(GEOShandle, geom);
    
//    type = GEOSGeomTypeId_r(GEOShandle, res);

    int ng = GEOSGetNumGeometries_r(GEOShandle, res);

//Rprintf("ng: %d, type: %d, %s\n", ng, type, GEOSGeomType_r(GEOShandle, res));

    char buf[BUFSIZ];

    PROTECT(id = NEW_CHARACTER(ng)); pc++;
    for (int i=0; i<ng; i++) {
        snprintf(buf, sizeof(buf), "%d", i);
        SET_STRING_ELT(id, i, COPY_TO_USER_STRING(buf));
    }

    GEOSGeom_destroy_r(GEOShandle, geom);

    ans = rgeos_convert_geos2R(env, res, p4s, id); 

    UNPROTECT(pc);
    return(ans);

}
#endif

