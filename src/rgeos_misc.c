#include "rgeos.h"


SEXP rgeos_area(SEXP env, SEXP obj, SEXP byid) {
    return( rgeos_miscfunc(env, obj, byid, &GEOSArea_r) );
}

SEXP rgeos_length(SEXP env, SEXP obj, SEXP byid) {
    return( rgeos_miscfunc(env, obj, byid, &GEOSLength_r) );
}


SEXP rgeos_miscfunc(SEXP env, SEXP obj, SEXP byid, p_miscfunc miscfunc) {

    SEXP ans;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSGeom geom = rgeos_convert_R2geos(env, obj);
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    int n = (LOGICAL_POINTER(byid)[0] && type == GEOS_GEOMETRYCOLLECTION) ? 
                GEOSGetNumGeometries_r(GEOShandle, geom) : 1;
    
    int pc=0;
    PROTECT(ans = NEW_NUMERIC(n)); pc++;

    GEOSGeom curgeom = geom;
    for(int i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_miscfunc: unable to get subgeometries");
        }
        
        double val;
        if (!miscfunc(GEOShandle, curgeom, &val))
            error("rgeos_miscfunc: unable to calculate");
            
        NUMERIC_POINTER(ans)[i] = val;
    }

    GEOSGeom_destroy_r(GEOShandle, geom);

    UNPROTECT(pc);
    return(ans);
}



SEXP rgeos_distance(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_distancefunc(env, spgeom1, spgeom2, byid, &GEOSDistance_r) );
}

SEXP rgeos_hausdorffdistance(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_distancefunc(env, spgeom1, spgeom2, byid, &GEOSHausdorffDistance_r) );
}

SEXP rgeos_distancefunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, p_distfunc distfunc) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSGeom geom1 = rgeos_convert_R2geos(env, spgeom1);
    int type1 = GEOSGeomTypeId_r(GEOShandle, geom1);
    GEOSGeom geom2;
    int type2;
    
    int sym_ans = FALSE;
    if (spgeom2 == R_NilValue) {
        sym_ans = TRUE;
        geom2 = geom1;
        type2 = GEOSGeomTypeId_r(GEOShandle, geom2);
    } else {
        geom2 = rgeos_convert_R2geos(env, spgeom2);
        type2 = GEOSGeomTypeId_r(GEOShandle, geom2);
    }

    int m = (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION) ?
                GEOSGetNumGeometries_r(GEOShandle, geom1) : 1;
    int n = (LOGICAL_POINTER(byid)[1] && type2 == GEOS_GEOMETRYCOLLECTION) ?
                GEOSGetNumGeometries_r(GEOShandle, geom2) : 1;
                
    if (m == -1) error("rgeos_distancefunc: invalid number of subgeometries in geometry 1");
    if (n == -1) error("rgeos_distancefunc: invalid number of subgeometries in geometry 2");

    int pc=0;
    SEXP ans;
    PROTECT(ans = NEW_NUMERIC(m*n)); pc++;

    GEOSGeom curgeom1 = geom1;
    GEOSGeom curgeom2 = geom2;
    for(int i=0; i<m; i++) {
        
        if ( m > 1) {
            curgeom1 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom1, i);
            if (curgeom1 == NULL) 
                error("rgeos_binpredfunc: unable to get subgeometries from geometry 1");
        }
        for(int j=0; j<n; j++) {
            if(sym_ans && j > i)
                break;
            
            if ( n > 1) {
                curgeom2 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom2, j);
                if (curgeom2 == NULL) 
                    error("rgeos_binpredfunc: unable to get subgeometries from geometry 2");
            }
            
            double dist;
            if (!distfunc(GEOShandle, curgeom1, curgeom2, &dist))
                error("rgeos_distancefunc: unable to calculate distance");

            NUMERIC_POINTER(ans)[n*i+j] = dist;
            if (sym_ans) NUMERIC_POINTER(ans)[n*j+i] = dist;
        }
    }
    
	if (LOGICAL_POINTER(byid)[0] || LOGICAL_POINTER(byid)[1]) {
        SEXP dims;
        PROTECT(dims = NEW_INTEGER(2)); pc++;
        INTEGER_POINTER(dims)[0] = n;
        INTEGER_POINTER(dims)[1] = m;
        setAttrib(ans, R_DimSymbol, dims);
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom1);
    if (!sym_ans)
        GEOSGeom_destroy_r(GEOShandle, geom2);
    
    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_hausdorffdistancedensify(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP densifyFrac, SEXP byid) {
    return( rgeos_distancedensifyfunc(env, spgeom1, spgeom2, densifyFrac, byid, &GEOSHausdorffDistanceDensify_r) );
}

SEXP rgeos_distancedensifyfunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP densifyFrac, SEXP byid, p_distdenfunc distfunc) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSGeom geom1 = rgeos_convert_R2geos(env, spgeom1);
    int type1 = GEOSGeomTypeId_r(GEOShandle, geom1);
    GEOSGeom geom2;
    int type2;
    
    int sym_ans = FALSE;
    if (spgeom2 == R_NilValue) {
        sym_ans = TRUE;
        geom2 = geom1;
        type2 = GEOSGeomTypeId_r(GEOShandle, geom2);
    } else {
        geom2 = rgeos_convert_R2geos(env, spgeom2);
        type2 = GEOSGeomTypeId_r(GEOShandle, geom2);
    }

    int m = (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION) ?
                GEOSGetNumGeometries_r(GEOShandle, geom1) : 1;
    int n = (LOGICAL_POINTER(byid)[1] && type2 == GEOS_GEOMETRYCOLLECTION) ?
                GEOSGetNumGeometries_r(GEOShandle, geom2) : 1;
                
    if (m == -1) error("rgeos_distancefunc: invalid number of subgeometries in geometry 1");
    if (n == -1) error("rgeos_distancefunc: invalid number of subgeometries in geometry 2");

    double frac = NUMERIC_POINTER(densifyFrac)[0];

    int pc=0;
    SEXP ans;
    PROTECT(ans = NEW_NUMERIC(m*n)); pc++;

    GEOSGeom curgeom1 = geom1;
    GEOSGeom curgeom2 = geom2;
    for(int i=0; i<m; i++) {
        
        if ( m > 1) {
            curgeom1 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom1, i);
            if (curgeom1 == NULL) 
                error("rgeos_binpredfunc: unable to get subgeometries from geometry 1");
        }
        for(int j=0; j<n; j++) {
            if(sym_ans && j > i)
                break;
            
            if ( n > 1) {
                curgeom2 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom2, j);
                if (curgeom2 == NULL) 
                    error("rgeos_binpredfunc: unable to get subgeometries from geometry 2");
            }
            
            double dist;
            if (!distfunc(GEOShandle, curgeom1, curgeom2, frac, &dist))
                error("rgeos_distancefunc: unable to calculate distance");

            NUMERIC_POINTER(ans)[n*i+j] = dist;
            if (sym_ans) NUMERIC_POINTER(ans)[n*j+i] = dist;
        }
    }
    
    if (n != 1 && m !=1) {
        SEXP dims;
        PROTECT(dims = NEW_INTEGER(2)); pc++;
        INTEGER_POINTER(dims)[0] = n;
        INTEGER_POINTER(dims)[1] = m;
        setAttrib(ans, R_DimSymbol, dims);
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom1);
    if (!sym_ans)
        GEOSGeom_destroy_r(GEOShandle, geom2);
    
    UNPROTECT(pc);
    return(ans);
}

#ifdef HAVE_NEARESTPOINTS
SEXP rgeos_nearestpoints(SEXP env, SEXP spgeom1, SEXP spgeom2) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    GEOSGeom geom1 = rgeos_convert_R2geos(env, spgeom1);
    GEOSGeom geom2 = rgeos_convert_R2geos(env, spgeom2);


    // Returns the closest points of the two geometries, 0 on exception.
    // The first point comes from geom1 geometry and
    // the second point comes from geom2.
    GEOSCoordSequence* s = GEOSNearestPoints_r(GEOShandle, geom1, geom2);

    SEXP coordmat;
    if (s == 0) {
        coordmat = R_NilValue;
    } else {
        coordmat = rgeos_CoordSeq2crdMat(env, s, 0, 0);
    }

    GEOSCoordSeq_destroy_r(GEOShandle, s);

    return(coordmat);
}
#endif
