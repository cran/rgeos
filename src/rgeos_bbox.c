#include "rgeos.h"

SEXP rgeos_geom2bbox(SEXP env, GEOSGeom geom) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    GEOSGeom bb = GEOSEnvelope_r(GEOShandle, geom);
    if (bb == NULL) return(R_NilValue);
    
    if (GEOSisEmpty_r(GEOShandle, bb) == 1) return(R_NilValue);
    
    GEOSCoordSeq s;
    int type = GEOSGeomTypeId_r(GEOShandle, bb);
    if (type == GEOS_POINT) {
        s = (GEOSCoordSeq) GEOSGeom_getCoordSeq_r(GEOShandle, bb);
        if (s == NULL) return(R_NilValue);
    } else {
        GEOSGeom bbER = (GEOSGeom) GEOSGetExteriorRing_r(GEOShandle, bb);
        if (bbER == NULL) return(R_NilValue);
        
        s = (GEOSCoordSeq) GEOSGeom_getCoordSeq_r(GEOShandle, bbER);
        if (s == NULL) return(R_NilValue);
    }
    
    unsigned int n;
    GEOSCoordSeq_getSize_r(GEOShandle, s, &n);
    if (n == 0) return(R_NilValue);

    int  pc=0;
    SEXP bbmat;
    PROTECT(bbmat = rgeos_CoordSeq2crdMat(env, s, 0, FALSE)); pc++;
    //PROTECT(bbmat = rgeos_CoordSeq2crdMat(env, s, (int) GEOSHasZ_r(GEOShandle, geom), FALSE)); pc++;
        
    double UX=-DBL_MAX, LX=DBL_MAX, UY=-DBL_MAX, LY=DBL_MAX;
    for (int i=0; i<n; i++) {
       if (NUMERIC_POINTER(bbmat)[i] > UX)   UX = NUMERIC_POINTER(bbmat)[i];
       if (NUMERIC_POINTER(bbmat)[i+n] > UY) UY = NUMERIC_POINTER(bbmat)[i+n];
       if (NUMERIC_POINTER(bbmat)[i] < LX)   LX = NUMERIC_POINTER(bbmat)[i];
       if (NUMERIC_POINTER(bbmat)[i+n] < LY) LY = NUMERIC_POINTER(bbmat)[i+n];
    }

    SEXP ans;    
    PROTECT(ans = NEW_NUMERIC(4)); pc++;
    NUMERIC_POINTER(ans)[0] = LX;
    NUMERIC_POINTER(ans)[1] = LY;
    NUMERIC_POINTER(ans)[2] = UX;
    NUMERIC_POINTER(ans)[3] = UY;
    
    SEXP dim;
    PROTECT(dim = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dim)[0] = 2;
    INTEGER_POINTER(dim)[1] = 2;
    setAttrib(ans, R_DimSymbol, dim);
    
    SEXP dimnames;
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 0, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 1, COPY_TO_USER_STRING("y"));
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("min"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("max"));
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    
    //GEOSCoordSeq_destroy_r(GEOShandle,s);
    UNPROTECT(pc);
    return(ans);
}
