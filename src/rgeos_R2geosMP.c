#include "rgeos.h"

// sp Polygons to fish soup geometry collection (multipoint) 
GEOSGeom rgeos_Polygons2MP(SEXP env, SEXP obj) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
    
    SEXP pls;
    PROTECT(pls = GET_SLOT(obj, install("Polygons"))); pc++;
    int npls = length(pls);
    
    int nn = 0;
    for (int i=0; i<npls; i++) {
        SEXP crdMat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
        SEXP dim = getAttrib(crdMat, R_DimSymbol);
        nn += (INTEGER_POINTER(dim)[0]-1);
    }

    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) nn, sizeof(GEOSGeom));

    for (int i=0, ii=0; i<npls; i++) {
        SEXP crdMat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
        int n = INTEGER_POINTER(getAttrib(crdMat, R_DimSymbol))[0];
        for (int j=0; j<(n-1); j++,ii++)
            geoms[ii] = rgeos_xy2Pt(env, NUMERIC_POINTER(crdMat)[j],NUMERIC_POINTER(crdMat)[j+n]);
    }

    GEOSGeom GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOINT, geoms, (unsigned int) nn);//VG FIXME
    if (GC == NULL)
        error("rgeos_Polygons2MP: collection not created");

    UNPROTECT(pc);
    return(GC);
}

// sp Polygon to fish soup geometry collection (multipoint) 
GEOSGeom rgeos_Polygon2MP(SEXP env, SEXP obj) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    SEXP crdMat = GET_SLOT(obj, install("coords"));
    SEXP dim = getAttrib(crdMat, R_DimSymbol);
    int nn = (INTEGER_POINTER(dim)[0]-1);

    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) nn, sizeof(GEOSGeom));

    for (int i=0; i<nn; i++)
        geoms[i] = rgeos_xy2Pt(env, NUMERIC_POINTER(crdMat)[i],NUMERIC_POINTER(crdMat)[i+nn]);

    GEOSGeom GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOINT, geoms, (unsigned int) nn);
    if (GC == NULL)
        error("rgeos_Polygon2MP: collection not created");

    return(GC);
}


// sp Lines to fish soup geometry collection (multipoint) 
GEOSGeom rgeos_Lines2MP(SEXP env, SEXP obj) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
    
    SEXP lines;
    PROTECT(lines = GET_SLOT(obj, install("Lines"))); pc++;
    int nlines = length(lines);
    
    int nn = 0;
    for (int i=0; i<nlines; i++) {
        SEXP crdMat = GET_SLOT(VECTOR_ELT(lines, i), install("coords"));
        SEXP dim = getAttrib(crdMat, R_DimSymbol);
        nn += (INTEGER_POINTER(dim)[0]-1);
    }

    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) nn, sizeof(GEOSGeom));
    for (int i=0, ii=0; i<nlines; i++) {
        SEXP crdMat = GET_SLOT(VECTOR_ELT(lines, i), install("coords"));
        SEXP dim = getAttrib(crdMat, R_DimSymbol);
        
        int n = INTEGER_POINTER(dim)[0];
        for (int j=0; j<(n-1); j++,ii++)
            geoms[ii] = rgeos_xy2Pt(env, NUMERIC_POINTER(crdMat)[j],NUMERIC_POINTER(crdMat)[j+n]);
    }

    GEOSGeom GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOINT, geoms, (unsigned int) nn);
    if (GC == NULL) {
        error("rgeos_Lines2MP: collection not created");
    }

    UNPROTECT(pc);
    return(GC);
}

