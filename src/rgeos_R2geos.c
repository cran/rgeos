#include "rgeos.h"
#include <string.h>

SEXP rgeos_double_translate(SEXP env, SEXP obj, SEXP id) {
    
    GEOSGeom geom = rgeos_convert_R2geos( env, obj);
    SEXP p4s = (obj == R_NilValue) ? R_NilValue : GET_SLOT(obj, install("proj4string"));
    
    SEXP ans = rgeos_convert_geos2R(env, geom, p4s, id); 
    
    return(ans);
}

GEOSGeom rgeos_convert_R2geos(SEXP env, SEXP obj) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    if (obj == R_NilValue)
        return(GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, NULL, 0));

    char classbuf[BUFSIZ];
    strcpy(classbuf, CHAR( STRING_ELT(GET_CLASS(obj), 0) ));
    
    //TODO - handle DataFrame classes gracefully
    if ( !strcmp( classbuf,"SpatialPoints") || !strcmp(classbuf,"SpatialPointsDataFrame") ) {
        return( rgeos_SpatialPoints2geospoint( env, obj) );
    } else if ( !strcmp(classbuf,"SpatialLines") || !strcmp(classbuf,"SpatialLinesDataFrame") ) {
        return( rgeos_SpatialLines2geosline( env, obj) );
    } else if ( !strcmp(classbuf,"SpatialRings") || !strcmp(classbuf,"SpatialRingsDataFrame") ) {
        return( rgeos_SpatialRings2geosring( env, obj) );
    } else if ( !strcmp(classbuf,"SpatialPolygons") || !strcmp(classbuf,"SpatialPolygonsDataFrame") ) {
        return( rgeos_SpatialPolygons2geospolygon( env, obj) );
    } else if ( !strcmp(classbuf,"SpatialCollections") ) {

        SEXP pointobj = GET_SLOT(obj, install("pointobj"));
        SEXP lineobj = GET_SLOT(obj, install("lineobj"));
        SEXP ringobj = GET_SLOT(obj, install("ringobj"));
        SEXP polyobj = GET_SLOT(obj, install("polyobj"));
        
        GEOSGeom GCs[] = {NULL,NULL,NULL,NULL};
        int cnts[] = {0,0,0,0};
        
        if (pointobj != R_NilValue) {
            GCs[0] = rgeos_SpatialPoints2geospoint(env, pointobj);
            cnts[0] = GEOSGetNumGeometries_r(GEOShandle, GCs[0]);
            cnts[0] = cnts[0] ? cnts[0] : 1;
        }
        if (lineobj != R_NilValue) {
            GCs[1] = rgeos_SpatialLines2geosline(env, lineobj);
            cnts[1] = GEOSGetNumGeometries_r(GEOShandle, GCs[1]);
            cnts[1] = cnts[1] ? cnts[1] : 1;
        }
        if (ringobj != R_NilValue) {
            GCs[2] = rgeos_SpatialRings2geosring(env, ringobj);
            cnts[2] = GEOSGetNumGeometries_r(GEOShandle, GCs[2]);
            cnts[2] = cnts[2] ? cnts[2] : 1;
        }
        if (polyobj != R_NilValue) {
            GCs[3] = rgeos_SpatialPolygons2geospolygon(env, polyobj);
            cnts[3] = GEOSGetNumGeometries_r(GEOShandle, GCs[3]);
            cnts[3] = cnts[3] ? cnts[3] : 1;        
        }
        int ng = cnts[0]+cnts[1]+cnts[2]+cnts[3];
        
        GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) ng, sizeof(GEOSGeom));
        int k=0;
        for(int i=0;i<4;i++) {
            
            if (cnts[i] == 0) continue;
            
            int n = GEOSGetNumGeometries_r(GEOShandle, GCs[i]);
            n = n ? n : 1;
            
            if (n == 1) {
                geoms[k] = GEOSGeom_clone_r(GEOShandle, GCs[i]);
                k++;
            } else if (n > 1) {
                for(int j=0; j<cnts[i]; j++, k++) {
                    const GEOSGeometry *curgeom = GEOSGetGeometryN_r(GEOShandle, GCs[i],j);
                    geoms[k] = GEOSGeom_clone_r(GEOShandle, curgeom);
                }
            }
            
            GEOSGeom_destroy_r(GEOShandle, GCs[i]);
        }
        
        return( GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, (unsigned int) ng) );
    }
    
    error("rgeos_convert_R2geos: invalid R class %s, unable to convert", classbuf);
    return(NULL); //should never get here, clears up warning
} 

// Spatial Points to geometry collection (Multipoints)
GEOSGeom rgeos_SpatialPoints2geospoint(SEXP env, SEXP obj) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc = 0;
    
    SEXP crds = GET_SLOT(obj, install("coords")); 
    SEXP dim = getAttrib(crds, install("dim")); 
    int n = INTEGER_POINTER(dim)[0];
    
    GEOSGeom GC = NULL;
    if ( n == 1 )
        GC = rgeos_xy2Pt(env, NUMERIC_POINTER(crds)[0], NUMERIC_POINTER(crds)[1]);
    else {
        SEXP ids;
        PROTECT(ids = VECTOR_ELT( getAttrib(crds, R_DimNamesSymbol), 0 ));pc++;
        
        if (ids == R_NilValue) {
            GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
            for (int j=0; j<n; j++) {
                geoms[j] = rgeos_xy2Pt(env, NUMERIC_POINTER(crds)[j], NUMERIC_POINTER(crds)[j+n]);            
                if (geoms[j] == NULL) 
					error("rgeos_SpatialPoints2geospoint: collection not created");
            }
        
            GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, (unsigned int) n);   
            //EJP, uncommented; loop now starting at 1; passes check:
            //for (int j=1; j<n; j++) 
            //    GEOSGeom_destroy_r(GEOShandle, geoms[j]);
            if (GC == NULL) 
				error("rgeos_SpatialPoints2geospoint: collection not created");
         
        } else {
            
            int *unique  = (int *) R_alloc((size_t) n, sizeof(int));
            int *unqcnt  = (int *) R_alloc((size_t) n, sizeof(int));
            int *whichid = (int *) R_alloc((size_t) n, sizeof(int));
            int nunq = 1;

            unique[0] = 0;
            unqcnt[0] = 1;
            whichid[0] = 0;
        
            for (int i=1; i<n; i++) {
            
                int match = FALSE;
                int j;
                for (j=0; j<nunq; j++) {
                    match = !strcmp( CHAR(STRING_ELT(ids, i)), CHAR(STRING_ELT(ids, unique[j])) );
                    if (match) break;
                }
            
                if (!match) {
                    unique[nunq] = i;
                    unqcnt[nunq] = 0;
                    nunq++;
                }
                unqcnt[j]++;
                whichid[i] = j;
            }
        
            GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) nunq, sizeof(GEOSGeom));
            for (int j=0; j<nunq; j++) {

                GEOSGeom *subgeoms = (GEOSGeom *) R_alloc((size_t) unqcnt[j], sizeof(GEOSGeom));
                for (int i=0; i<unqcnt[j]; i++) subgeoms[i] = NULL;

                int k=0;
                for (int i=0; i<n; i++) {
                    if (whichid[i] == j) {
                        subgeoms[k] = rgeos_xy2Pt(env, NUMERIC_POINTER(crds)[i], NUMERIC_POINTER(crds)[i+n]);
                        k++;
                    }
                }
                
                geoms[j] = (k == 1) ? subgeoms[0] : 
                            GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOINT, subgeoms, (unsigned int) unqcnt[j]);//VG FIXME
                
                if (geoms[j] == NULL) 
                    error("rgeos_SpatialPoints2geospoint: collection not created");
            }
            
            GC = (nunq == 1) ? geoms[0] :
                   GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, (unsigned int) nunq);//VG FIXME
            
            if (GC == NULL)
                error("rgeos_SpatialPoints2geospoint: collection not created");
        }
    } 
// Rprintf("n: %d, GC %s\n", n, GEOSGeomType_r(GEOShandle, GC));
    
    UNPROTECT(pc);
    return(GC);
}

// SpatialLines class to geometry collection
GEOSGeom rgeos_SpatialLines2geosline(SEXP env, SEXP obj) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc = 0;
    
    SEXP lines;
    PROTECT(lines = GET_SLOT(obj, install("lines"))); pc++;
    int nlines = length(lines);
    
    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) nlines, sizeof(GEOSGeom));
    
    for (int i=0; i<nlines; i++) {
        SEXP Lines = VECTOR_ELT(lines, i);
        geoms[i] = rgeos_Lines2geosline(env, Lines);
    }
    
    // If there is only one line collection return multiline not GC
    GEOSGeom GC = (nlines == 1) ? geoms[0]
                    : GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, (unsigned int) nlines);//VG FIXME
    if (GC == NULL) {
            GEOSGeom_destroy_r(GEOShandle, GC);
            char* buf = get_errbuf();
            error(buf);
    }
    
    UNPROTECT(pc);
    return(GC);
}

// Lines class to geometry collection (Multilinestring)
GEOSGeom rgeos_Lines2geosline(SEXP env, SEXP obj) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
    
    SEXP lns;
    PROTECT(lns = GET_SLOT(obj, install("Lines"))); pc++;
    int nlns = length(lns);

    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) nlns, sizeof(GEOSGeom));

    for (int i=0; i<nlns; i++) {
        SEXP crdMat = GET_SLOT(VECTOR_ELT(lns, i), install("coords"));
        
        if (crdMat == R_NilValue) {
            geoms[i] = GEOSGeom_createLineString_r(GEOShandle, NULL);
        } else {
            SEXP dim = getAttrib(crdMat, R_DimSymbol);
            geoms[i] = rgeos_crdMat2LineString(env, crdMat, dim);
        }
    }
    
    GEOSGeom GC = (nlns == 1) ? geoms[0]
                    : GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTILINESTRING, geoms, (unsigned int) nlns); //VG FIXME
    if (GC == NULL) {
            GEOSGeom_destroy_r(GEOShandle, GC);
            char* buf = get_errbuf();
            error(buf);
    }
    
    UNPROTECT(pc);
    return(GC);
}


// Spatial polygons to geometry collection (multipolygon)
GEOSGeom rgeos_SpatialPolygons2geospolygon(SEXP env, SEXP obj) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
    
    SEXP pls;
    PROTECT(pls = GET_SLOT(obj, install("polygons"))); pc++;
    int npls = length(pls);
    
    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));
    
    for (int i=0; i<npls; i++)
        geoms[i] = rgeos_Polygons2geospolygon(env, VECTOR_ELT(pls, i));
    
    GEOSGeom GC = (npls == 1) ? geoms[0]
                    : GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, (unsigned int) npls);//VG FIXME
    
    UNPROTECT(pc);
    return(GC);
}


GEOSGeom rgeos_Polygons2geospolygon(SEXP env, SEXP obj) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
    
    SEXP pls;
    PROTECT(pls = GET_SLOT(obj, install("Polygons"))); pc++;
    int npls = length(pls);
    
    SEXP comm;
    PROTECT(comm = SP_PREFIX(comment2comm)(obj)); pc++;

    GEOSGeom GC;
    if (comm == R_NilValue) {

        GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

        int warned = FALSE;
        int n = 0;
        
        for (int i=0; i<npls; i++) {
            SEXP crdMat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
            
            int hole = LOGICAL_POINTER( GET_SLOT(VECTOR_ELT(pls, i), install("hole")) )[0];
            if (hole) {
                if (!warned) {
                    warning("Polygons object missing comment attribute ignoring hole(s). See function createSPComment.");
                    warned = TRUE;
                }
                continue;
            }
            
            geoms[n] = (crdMat == R_NilValue)
                         ? GEOSGeom_createPolygon_r(GEOShandle, NULL, NULL, (unsigned int) 0)
                         : rgeos_crdMat2Polygon(env, crdMat, getAttrib(crdMat, R_DimSymbol));
            n++;
        }
        
        GC = (n == 1) ? geoms[0]
              : GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON, geoms, (unsigned int) n);
        
    } else {

        int nErings = length(comm);
        int ncomm = 0;
        for (int i=0; i<nErings; i++) ncomm += length(VECTOR_ELT(comm, i));
// Rprintf("npls %d, ncomm %d\n", npls, ncomm);
        if (npls != ncomm)
            error("lengths of comment and Polygons slot differ");

        GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) nErings, sizeof(GEOSGeom));
        
        for (int i=0; i<nErings; i++)
            geoms[i] = rgeos_Polygons_i_2Polygon(env, pls, VECTOR_ELT(comm, i));
        
        GC = (nErings == 1) ? geoms[0]
               : GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON, geoms, (unsigned int) nErings);
    }
    if (GC == NULL) {
            GEOSGeom_destroy_r(GEOShandle, GC);
            char* buf = get_errbuf();
            error(buf);
    }
    
    UNPROTECT(pc);
    return(GC);
}


GEOSGeom rgeos_Polygons_i_2Polygon(SEXP env, SEXP pls, SEXP vec) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    int n = length(vec);
    int i = INTEGER_POINTER(vec)[0]-R_OFFSET;
    GEOSGeom pol;
    SEXP mat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
    if (mat == R_NilValue) {
        if (n != 1) error("Empty polygons should not have holes");
        pol = GEOSGeom_createLinearRing_r(GEOShandle, NULL);
    } else {
        pol = rgeos_crdMat2LinearRing(env, mat, getAttrib(mat, R_DimSymbol));
    }
    if (pol == NULL) {
        GEOSGeom_destroy_r(GEOShandle, pol);
        char* buf = get_errbuf();
        error(buf);
    }
    
    GEOSGeom res = NULL;
//Rprintf("rgeos_Polygons_i_2Polygon: n: %d\n", n);
    if (n == 1) {
//Rprintf("in n==1 branch\n");
        res = GEOSGeom_createPolygon_r(GEOShandle, pol, NULL, (unsigned int) 0);//VG FIXME
        if (res == NULL) {
            GEOSGeom_destroy_r(GEOShandle, pol);
            GEOSGeom_destroy_r(GEOShandle, res);
            char* buf = get_errbuf();
            error(buf);
        }
    } else if (n > 1) {
//Rprintf("in n>1 branch\n");
        GEOSGeom *holes = (GEOSGeom *) R_alloc((size_t) (n-1), sizeof(GEOSGeom));
        for (int j=1; j<n; j++) {
            i = INTEGER_POINTER(vec)[j]-R_OFFSET;
            mat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
            
            if (mat == R_NilValue) {
                holes[j-1] = GEOSGeom_createLinearRing_r(GEOShandle, NULL);
            } else {
                holes[j-1] = rgeos_crdMat2LinearRing(env, mat, getAttrib(mat, R_DimSymbol));
            }
        }
        res = GEOSGeom_createPolygon_r(GEOShandle, pol, holes,
            (unsigned int) (n-1)); //FIXME not in branch but throws leak
        if (res == NULL) {
            GEOSGeom_destroy_r(GEOShandle, pol);
            GEOSGeom_destroy_r(GEOShandle, res);
            char* buf = get_errbuf();
            error(buf);
        }
    }
    
    if (res == NULL)
        error("rgeos_Polygons_i_2Polygon: Polygon not created");
    
    return(res);
}


// SpatialRings class to geometry collection
GEOSGeom rgeos_SpatialRings2geosring(SEXP env, SEXP obj) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    int pc = 0;
    SEXP rings;
    PROTECT(rings = GET_SLOT(obj, install("rings"))); pc++;
    int nrings = length(rings);

    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) nrings, sizeof(GEOSGeom));
    for (int i=0; i<nrings; i++) {    
        SEXP crdMat = GET_SLOT(VECTOR_ELT(rings, i), install("coords"));
        
        if (crdMat == R_NilValue) {
            geoms[i] = GEOSGeom_createLinearRing_r(GEOShandle, NULL);
        } else {
            SEXP dim = getAttrib(crdMat, R_DimSymbol);
            geoms[i] = rgeos_crdMat2LinearRing(env, crdMat, dim);
        }
    }
    
    GEOSGeom GC = (nrings == 1) ? geoms[0]
                    : GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, (unsigned int) nrings);
    
    if (GC == NULL)
        error("rgeos_SpatialRings2geosring: collection not created");
    
    UNPROTECT(pc);
    return(GC);
}

