#include "rgeos.h"

GEOSCoordSeq rgeos_crdMat2CoordSeq(SEXP env, SEXP mat, SEXP dim) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    int n = INTEGER_POINTER(dim)[0];
    int m = INTEGER_POINTER(dim)[1];

    if (m != 2) error("Only 2D geometries permitted");

    GEOSCoordSeq s = GEOSCoordSeq_create_r(GEOShandle, (unsigned int) n, (unsigned int) m);
    if (s == NULL) error("rgeos_crdMat2CoordSeq: NULL GEOSCoordSeq");

    double scale = getScale(env);
    for(int i=0; i<n; i++) {
        double val;
        val = makePrecise( NUMERIC_POINTER(mat)[i], scale);
        if (GEOSCoordSeq_setX_r(GEOShandle, s, (unsigned int) i, val) == 0) {
            GEOSCoordSeq_destroy_r(GEOShandle, s);
            error("rgeos_crdMat2CoordSeq: X not set for %d", i);
        }
        val = makePrecise( NUMERIC_POINTER(mat)[i+n], scale);
        if (GEOSCoordSeq_setY_r(GEOShandle, s, (unsigned int) i, val) == 0) {
            GEOSCoordSeq_destroy_r(GEOShandle, s);
            error("rgeos_crdMat2CoordSeq: Y not set for %d", i);
        }
    }

    return(s);
}

GEOSGeom rgeos_crdMat2LineString(SEXP env, SEXP mat, SEXP dim) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSCoordSeq s = rgeos_crdMat2CoordSeq(env, mat, dim);    
    GEOSGeom gl = GEOSGeom_createLineString_r(GEOShandle, s);//VG FIXME
    
    if (gl == NULL) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        char* buf = get_errbuf();
        error(buf);
//        error("rgeos_crdMat2LineString: lineString not created");
    }
    return(gl);
}


GEOSGeom rgeos_crdMat2LinearRing(SEXP env, SEXP mat, SEXP dim) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSCoordSeq s = rgeos_crdMat2CoordSeq(env, mat, dim);

    GEOSGeom gl = GEOSGeom_createLinearRing_r(GEOShandle, s);
    if (gl == NULL) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_crdMat2LinearRing: linearRing not created");
    }
    /*
	if ((int) GEOSisValid_r(GEOShandle, gl) == 1) {
        if (GEOSNormalize_r(GEOShandle, gl) == -1)
            warning("rgeos_crdMat2LinearRing: normalization failure");
    } else {
        warning("rgeos_crdMat2LinearRing: validity failure");
    }*/

    return(gl);
}

GEOSGeom rgeos_crdMat2Polygon(SEXP env, SEXP mat, SEXP dim) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSGeom g1 = rgeos_crdMat2LinearRing(env, mat, dim);
    GEOSGeom p1 = GEOSGeom_createPolygon_r(GEOShandle, g1, NULL, (unsigned int) 0);
    if (p1 == NULL) {
        GEOSGeom_destroy_r(GEOShandle, g1);
        error("rgeos_crdMat2Polygon: Polygon not created");
    }

    return(p1);
}

SEXP rgeos_crdMatFixDir(SEXP crd, int hole) {
    
    double area = 0.0;
    int n = length(crd)/2, pc=0;
    for(int i=1; i<n;i++) {
        area += (NUMERIC_POINTER(crd)[i] - NUMERIC_POINTER(crd)[i-1])*
                (NUMERIC_POINTER(crd)[i+n] + NUMERIC_POINTER(crd)[i+n-1]);
    }
    
    int cw = (area > 0) ? TRUE : FALSE;
    
    if ( (hole && cw) || (!hole && !cw) ) {
        SEXP newcrd;
        PROTECT( newcrd = NEW_NUMERIC(n*2) ); pc++;
        for(int i=0; i<n;i++) {
            NUMERIC_POINTER(newcrd)[i] = NUMERIC_POINTER(crd)[n-i-1];
            NUMERIC_POINTER(newcrd)[n+i] = NUMERIC_POINTER(crd)[n+n-i-1];
        }
        
        PROTECT(crd = rgeos_formatcrdMat(newcrd, n)); pc++;
    
        UNPROTECT(pc);
    }
    
    //Rprintf("HERE cw:%d hole:%d\n",cw,hole);
    return(crd);
}

SEXP rgeos_CoordSeq2crdMat(SEXP env, const GEOSCoordSequence *s, int HasZ, int rev) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    unsigned int n, m=0;
    if (GEOSCoordSeq_getSize_r(GEOShandle, s, &n) == 0 ||
        GEOSCoordSeq_getDimensions_r(GEOShandle, s, &m) == 0) {
        error("rgeos_CoordSeq2crdMat: unable to get size and or get dimension of Coord Seq");
    }
    
    if (m == 3 && HasZ)
        warning("rgeos_CoordSeq2crdMat: only 2D coordinates respected");
    
    int pc=0;
    SEXP crd;
    PROTECT(crd = NEW_NUMERIC(n*2)); pc++;
    
    double scale = getScale(env);
    for (int i=0; i<n; i++){
        int ii = (rev) ? ((int) (n) -1)-i : i;
        
        double x=0.0, y=0.0;
        if (GEOSCoordSeq_getX_r(GEOShandle, s, (unsigned int) i, &x) == 0 ||
            GEOSCoordSeq_getY_r(GEOShandle, s, (unsigned int) i, &y) == 0) {
            error("rgeos_CoordSeq2crdMat: unable to get X and or Y value from Coord Seq");
        }
        NUMERIC_POINTER(crd)[ii]    = makePrecise(x, scale);
        NUMERIC_POINTER(crd)[ii+ (int) n]  = makePrecise(y, scale);
    }

    SEXP ans;
    PROTECT(ans = rgeos_formatcrdMat(crd, (int) n));pc++;
    
    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_geospoint2crdMat(SEXP env, GEOSGeom geom, SEXP idlist, int ntotal, int type) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    int m = (type == GEOS_GEOMETRYCOLLECTION) ? GEOSGetNumGeometries_r(GEOShandle, geom) : 1;
    if (m == -1) error("rgeos_geospoint2crdMat: invalid number of geometries");
    
    int pc=0;
    SEXP mat;
    PROTECT(mat = NEW_NUMERIC(ntotal*2)); pc++;

    SEXP ids = R_NilValue;
    if (idlist != R_NilValue) {/* FIXME RSB */
        PROTECT(ids = NEW_CHARACTER(ntotal)); pc++;
    }

    int k=0;
    double scale=getScale(env);
    
/*    int curtype = type; RSB 120624 */
    
    for(int j = 0; j<m; j++) {
        
        GEOSGeom curgeom = (type == GEOS_GEOMETRYCOLLECTION) ? 
                                (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, j) :
                                geom;
        if (curgeom == NULL) error("rgeos_geospoint2crdMat: unable to get sub geometry");
        
        int curtype = GEOSGeomTypeId_r(GEOShandle, curgeom);
        int n = GEOSGetNumGeometries_r(GEOShandle, curgeom);
        if (n == -1) error("rgeos_geospoint2crdMat: invalid number of geometries");
        n = n ? n : 1;
        
        char idbuf[BUFSIZ];
        if (idlist != R_NilValue) /* FIXME RSB */
            strcpy(idbuf, CHAR(STRING_ELT(idlist, j)));
        
        for (int i=0; i<n; i++) {
            GEOSGeom subgeom = (curtype == GEOS_MULTIPOINT && !GEOSisEmpty_r(GEOShandle, curgeom)) ?
                                (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, curgeom, i) :
                                curgeom;
            if (subgeom == NULL) error("rgeos_geospoint2crdMat: unable to get sub geometry");
            
            if (GEOSisEmpty_r(GEOShandle, subgeom) == 1) {
                NUMERIC_POINTER(mat)[k]        = NA_REAL;
                NUMERIC_POINTER(mat)[k+ntotal] = NA_REAL;
            } else {
                double x,y;
                rgeos_Pt2xy(env, subgeom, &x, &y);
                
                NUMERIC_POINTER(mat)[k]        = makePrecise(x, scale);
                NUMERIC_POINTER(mat)[k+ntotal] = makePrecise(y, scale);
            }
            if (idlist != R_NilValue) /* FIXME RSB */
                SET_STRING_ELT(ids, k, COPY_TO_USER_STRING(idbuf));
            
            k++;
        }
    }
    
    SEXP ans;
    PROTECT(ans = rgeos_formatcrdMat(mat,ntotal));pc++;
    
    if (idlist != R_NilValue) { /* FIXME RSB */
        SEXP dimnames;
        PROTECT(dimnames = getAttrib(ans, R_DimNamesSymbol));pc++;
        SET_VECTOR_ELT(dimnames, 0, ids);
        setAttrib(ans, R_DimNamesSymbol, dimnames);
    }
    
    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_formatcrdMat( SEXP crdMat, int n ) {
    int pc = 0;
    
    SEXP dims;
    PROTECT(dims = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dims)[0] = n;
    INTEGER_POINTER(dims)[1] = 2;
    
    SEXP dimnames;
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("y"));
    
    setAttrib(crdMat, R_DimSymbol, dims);
    setAttrib(crdMat, R_DimNamesSymbol, dimnames);
    
    UNPROTECT(pc);
    return(crdMat);
}



GEOSGeom rgeos_xy2Pt(SEXP env, double x, double y) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    GEOSGeom gl;
    GEOSCoordSeq s;
    
    if (ISNA(x) && ISNA(y)) {
        gl = GEOSGeom_createPoint_r(GEOShandle, NULL);
    } else {
        s = GEOSCoordSeq_create_r(GEOShandle, (unsigned int) 1, (unsigned int) 2);
        if (s == NULL) error("rgeos_xy2Pt: NULL GEOSCoordSeq");

        if (GEOSCoordSeq_setX_r(GEOShandle, s, 0, x) == 0) {
            GEOSCoordSeq_destroy_r(GEOShandle, s);
            error("rgeos_xy2Pt: X not set");
        }
        if (GEOSCoordSeq_setY_r(GEOShandle, s, 0, y) == 0) {
            GEOSCoordSeq_destroy_r(GEOShandle, s);
            error("rgeos_xy2Pt: Y not set");
        }
        
        gl = GEOSGeom_createPoint_r(GEOShandle, s);//VG FIXME
    }
    
    if (gl == NULL) { // EJP: destroy NULL pointer?
        //GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_xy2Pt: point not created");
    }
    
    return(gl);
}

void rgeos_Pt2xy(SEXP env, GEOSGeom point, double *x, double *y) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    if (GEOSisEmpty_r(GEOShandle, point)) {
        *x = NA_REAL;
        *y = NA_REAL;
        return;
    }
    
    int type = GEOSGeomTypeId_r(GEOShandle, point);
    if (type != GEOS_POINT)
        error("rgeos_Pt2xy: invalid geometry type, only accepts POINT type");
    
    GEOSCoordSeq s = (GEOSCoordSeq) GEOSGeom_getCoordSeq_r(GEOShandle, point);
    if (s == NULL) error("rgeos_Pt2xy: unable to get coord seq");

    if (GEOSCoordSeq_getX_r(GEOShandle, s, (unsigned int) 0, x) == 0 ||
        GEOSCoordSeq_getY_r(GEOShandle, s, (unsigned int) 0, y) == 0 ) {
    
        error("rgeos_Pt2xy: unable to get X and or Y value from coord seq");
    }   

}

