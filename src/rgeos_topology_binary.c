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

int GEOSTopologicalDimension_r(GEOSContextHandle_t extHandle,
    const GEOSGeometry* g) {
    int gType, res=-1;
    gType = GEOSGeomTypeId_r(extHandle, g);
    if (gType == GEOS_POINT || gType == GEOS_MULTIPOINT) res = 0;
    else if (gType == GEOS_LINESTRING || gType == GEOS_MULTILINESTRING) res = 1;
    else if (gType == GEOS_POLYGON || gType == GEOS_MULTIPOLYGON) res = 2;
    return(res);
}

SEXP rgeos_binarytopologyfunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids, p_bintopofunc bintopofunc) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int min_tds = INTEGER_POINTER(getAttrib(byid,
        install("min_tds")))[0];
    int drop_lower_td = LOGICAL_POINTER(getAttrib(byid,
        install("drop_lower_td")))[0];
    int uU = LOGICAL_POINTER(getAttrib(byid,
        install("unaryUnion_if_byid_false")))[0];
    int drop_me = FALSE,// k_type, 
        k_empty;
    int thistd=-1;

    
    // Default to using spgeom1's proj4string
    SEXP p4s = GET_SLOT(spgeom1, install("proj4string"));
    
    GEOSGeom geom1 = rgeos_convert_R2geos(env, spgeom1);
#ifdef HAVE_UNARYUNION
    if (!LOGICAL_POINTER(byid)[0] && uU) {
        geom1 = GEOSUnaryUnion_r(GEOShandle, geom1);
    }
#endif
    int type1 = GEOSGeomTypeId_r(GEOShandle, geom1);

    GEOSGeom geom2 = rgeos_convert_R2geos(env, spgeom2);
#ifdef HAVE_UNARYUNION
    if (!LOGICAL_POINTER(byid)[1] && uU) {
        geom2 = GEOSUnaryUnion_r(GEOShandle, geom2);
    }
#endif
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
            
            thisgeom = bintopofunc(GEOShandle, curgeom1, curgeom2);//VG FIXME
            if (thisgeom == NULL) {
                GEOSGeom_destroy_r(GEOShandle, geom1);
                GEOSGeom_destroy_r(GEOShandle, geom2);
                char* buf = get_errbuf();
                error(buf);
            }
            if (!GEOSisEmpty_r(GEOShandle, thisgeom)) {
 // conditionally drop returned objects with lower topological dimension
 // than minimum TD of input objects
               drop_me = FALSE;
               if (drop_lower_td) {
                   thistd = GEOSTopologicalDimension_r(GEOShandle, thisgeom);
                   if (thistd >= 0) {
                       if (thistd < min_tds) drop_me = TRUE;
                   } else {
                       if (GEOSGeomTypeId_r(GEOShandle, thisgeom)
                            == GEOS_GEOMETRYCOLLECTION) {
                            int nsGC = GEOSGetNumGeometries_r(GEOShandle,
                                thisgeom);
                            GEOSGeom *kgeoms = (GEOSGeom *) R_alloc(
                                (size_t) nsGC, sizeof(GEOSGeom));
                            for (k1=0, kk=0; k1<nsGC; k1++) {
                              kgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle,
                                thisgeom, k1);
                              thistd = GEOSTopologicalDimension_r(GEOShandle,
                                kgeom);
                              k_empty = GEOSisEmpty_r(GEOShandle, kgeom);
                              if (!k_empty && thistd == min_tds) {
                                kgeoms[kk] = kgeom;
//Rprintf("%d %d %d %s\n", k1, kk, GEOSGeomTypeId_r(GEOShandle, kgeom), GEOSGeomType_r(GEOShandle, kgeom));
                                kk++;
                            }
                        }
                        if (kk == 0) {
                            drop_me = TRUE;
                       } else {
                              if (kk > 1) {
                                  if (min_tds == 0) {
                                    thisgeom = GEOSGeom_createCollection_r(
                                      GEOShandle, GEOS_MULTIPOINT, 
                                      kgeoms, (unsigned int) kk);
                                } else if (min_tds == 1) {
                                    thisgeom = GEOSGeom_createCollection_r(
                                      GEOShandle, GEOS_MULTILINESTRING, 
                                      kgeoms, (unsigned int) kk);
                                } else if (min_tds == 2) {
                                    thisgeom = GEOSGeom_createCollection_r(
                                      GEOShandle, GEOS_MULTIPOLYGON, 
                                      kgeoms, (unsigned int) kk);
                                }
                            } else {
                                thisgeom = kgeoms[0];
                            }
                        }

                       } else {
                           drop_me = TRUE;
                       }
                   }
                }
                if (!drop_me) {
                    geoms[k] = thisgeom;
                    SET_STRING_ELT(ids, k, STRING_ELT(ids, i*n+j));
//Rprintf("%d: %d %d %s %d\n", k, GEOSisEmpty_r(GEOShandle, geoms[k]), GEOSGeomTypeId_r(GEOShandle, geoms[k]), GEOSGeomType_r(GEOShandle, geoms[k]), GEOSGetNumGeometries_r(GEOShandle, geoms[k]));
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
    GEOSGeom res = (k > 1) ? GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, (unsigned int) k)//VG FIXME
                             : geoms[0];
    
//Rprintf("res is %s\n", GEOSGeomType_r(GEOShandle, res));

    return( rgeos_convert_geos2R(env, res, p4s, ids) );
}

