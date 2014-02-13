#include "rgeos.h"

SEXP rgeos_difference(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids) {
    return( rgeos_binarytopologyfunc(env, spgeom1, spgeom2, byid, ids, &GEOSDifference_r) );
}
SEXP rgeos_symdifference(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids) {
    return( rgeos_binarytopologyfunc(env, spgeom1, spgeom2, byid, ids, &GEOSSymDifference_r) );
}
SEXP rgeos_intersection(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids) {
    return( rgeos_binarytopologyfunc(env, spgeom1, spgeom2, byid, ids, &GEOSIntersection_r) );
}
SEXP rgeos_union(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids) {
    return( rgeos_binarytopologyfunc(env, spgeom1, spgeom2, byid, ids, &GEOSUnion_r) );
}

SEXP rgeos_binarytopologyfunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids, p_bintopofunc bintopofunc) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int both_poly = LOGICAL_POINTER(findVarInFrame(env,
        install("both_poly")))[0];
    int drop_not_poly = LOGICAL_POINTER(findVarInFrame(env,
        install("drop_not_poly")))[0];
    int drop_me = FALSE, k_type, k_empty;

    
    // Default to using spgeom1's proj4string
    SEXP p4s = GET_SLOT(spgeom1, install("proj4string"));
    
    GEOSGeom geom1 = rgeos_convert_R2geos(env, spgeom1);
    int type1 = GEOSGeomTypeId_r(GEOShandle, geom1);

    GEOSGeom geom2 = rgeos_convert_R2geos(env, spgeom2);
    int type2 = GEOSGeomTypeId_r(GEOShandle, geom2);

    int m = (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION) ? 
            GEOSGetNumGeometries_r(GEOShandle, geom1) : 1;

    int n = (LOGICAL_POINTER(byid)[1] && type2 == GEOS_GEOMETRYCOLLECTION) ?
            GEOSGetNumGeometries_r(GEOShandle, geom2) : 1;
    
    if (m == -1) error("rgeos_bintopofunc: invalid number of subgeometries in geometry 1");
    if (n == -1) error("rgeos_bintopofunc: invalid number of subgeometries in geometry 2");

    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) (m*n), sizeof(GEOSGeom));
    GEOSGeom thisgeom;
    GEOSGeom kgeom;
//Rprintf("m=%d n=%d\n", m, n);
//Rprintf("spgeom1 is %s\n", GEOSGeomType_r(GEOShandle, geom1));
//Rprintf("spgeom2 is %s\n", GEOSGeomType_r(GEOShandle, geom2));


    int k=0, k1=0, kk=0;
    for(int i=0; i<m; i++) {

        const GEOSGeometry *curgeom1 = (m > 1) ? GEOSGetGeometryN_r(GEOShandle, geom1, i) : geom1;
        if (curgeom1 == NULL) 
            error("rgeos_bintopofunc: unable to get subgeometries from geometry 1");
        
        for(int j=0; j<n; j++) {
        
            const GEOSGeometry *curgeom2 = (n > 1) ? GEOSGetGeometryN_r(GEOShandle, geom2, j) : geom2;
            if (curgeom2 == NULL) 
                error("rgeos_bintopofunc: unable to get subgeometries from geometry 2");
            
            thisgeom = bintopofunc(GEOShandle, curgeom1, curgeom2);
            if (thisgeom == NULL)
                error("rgeos_bintopofunc: topology function failed");
            if (!GEOSisEmpty_r(GEOShandle, thisgeom)) {
 // conditionally drop non-polygon returned objects
               drop_me = FALSE;
               if (both_poly && drop_not_poly) {
                    if (GEOSGeomTypeId_r(GEOShandle, thisgeom)
                        == GEOS_POINT) {
                        drop_me = TRUE;
                    } else if (GEOSGeomTypeId_r(GEOShandle, thisgeom)
                        == GEOS_MULTIPOINT) {
                        drop_me = TRUE;
                    } else if (GEOSGeomTypeId_r(GEOShandle, thisgeom)
                        == GEOS_LINESTRING) {
                        drop_me = TRUE;
                    } else if (GEOSGeomTypeId_r(GEOShandle, thisgeom)
                        == GEOS_MULTILINESTRING) {
                        drop_me = TRUE;
                    } else if (GEOSGeomTypeId_r(GEOShandle, thisgeom)
                        == GEOS_GEOMETRYCOLLECTION) {
                        int nsGC = GEOSGetNumGeometries_r(GEOShandle, thisgeom);
                        GEOSGeom *kgeoms = (GEOSGeom *) R_alloc(
                            (size_t) nsGC, sizeof(GEOSGeom));
                        for (k1=0, kk=0; k1<nsGC; k1++) {
                            kgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle,
                                thisgeom, k1);
                            k_type = GEOSGeomTypeId_r(GEOShandle, kgeom);
                            k_empty = GEOSisEmpty_r(GEOShandle, kgeom);
                            if (!k_empty && (k_type == GEOS_POLYGON ||
                                k_type == GEOS_MULTIPOLYGON)) {
                                kgeoms[kk] = kgeom;
                                kk++;
                            }
                        }
                        if (kk == 0) {
                            drop_me = TRUE;
                        } else {
                            thisgeom = (kk > 1) ? GEOSGeom_createCollection_r(
                                GEOShandle, GEOS_GEOMETRYCOLLECTION, 
                                kgeoms, (unsigned int) kk) : kgeoms[0];
                        }
                    }
                }
                if (!drop_me) {
                    geoms[k] = thisgeom;
                    SET_STRING_ELT(ids, k, STRING_ELT(ids, i*n+j));
//            Rprintf("%d: %d %d %s %d\n", k, GEOSisEmpty_r(GEOShandle, geoms[k]), GEOSGeomTypeId_r(GEOShandle, geoms[k]), GEOSGeomType_r(GEOShandle, geoms[k]), GEOSGetNumGeometries_r(GEOShandle, geoms[k]));
                    k++;
                }
            }
            
            //if (GEOSisEmpty_r(GEOShandle, geomsk)) continue;
        }
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom1);
    GEOSGeom_destroy_r(GEOShandle, geom2);
//Rprintf("m=%d n=%d k=%d\n", m, n, k);

    if (k == 0)
        return(R_NilValue);
    GEOSGeom res = (k > 1) ? GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, (unsigned int) k)
                             : geoms[0];
    
//Rprintf("res is %s\n", GEOSGeomType_r(GEOShandle, res));

    return( rgeos_convert_geos2R(env, res, p4s, ids) );
}

