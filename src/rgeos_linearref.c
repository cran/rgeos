#include "rgeos.h"

// Return distance of points 'spppoints' projected on 'spgeom' from origin
// of 'spgeom'. Geometry 'spgeom' must be a lineal geometry
SEXP rgeos_project(SEXP env, SEXP spgeom, SEXP sppoint, SEXP normalized) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    GEOSGeom geom = rgeos_convert_R2geos(env, spgeom);

    SEXP crds = GET_SLOT(sppoint, install("coords"));
    SEXP dim = getAttrib(crds, install("dim"));

    int nlines = length(GET_SLOT(spgeom, install("lines")));
    if (nlines < 1) {
        error("rgeos_project: invalid number of lines");
    }

    int n = INTEGER_POINTER(dim)[0];
    if (n < 1) {
        error("rgeos_project: invalid number of points");
    }

    int pc = 0;
    SEXP ans;
    PROTECT(ans = NEW_NUMERIC(n)); pc++;

    GEOSGeom p;

    // select projection function (normalized/unnormalized)
    double GEOS_DLL (*proj_fun)(GEOSContextHandle_t,
                                const GEOSGeometry*,
                                const GEOSGeometry*);

    if (LOGICAL_POINTER(normalized)[0]) {
        proj_fun = &GEOSProjectNormalized_r;
    } else {
        proj_fun = &GEOSProject_r;
    }

    // project points to line geometry
    for (int i = 0; i < n; i++) {

        p = rgeos_xy2Pt(env,
                        NUMERIC_POINTER(crds)[i],
                        NUMERIC_POINTER(crds)[i+n]);

        NUMERIC_POINTER(ans)[i] = (*proj_fun)(GEOShandle, geom, p);
    }

    GEOSGeom_destroy_r(GEOShandle, geom);
    GEOSGeom_destroy_r(GEOShandle, p);

    UNPROTECT(pc);

    return(ans);
}


// Return closest point to given distance within geometry.
// 'spgeom' must be a LineString
SEXP rgeos_interpolate(SEXP env, SEXP spgeom, SEXP d, SEXP normalized) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    GEOSGeom geom = rgeos_convert_R2geos(env, spgeom);

    GEOSGeom res_geos;
    double dist;

    int nlines = length(GET_SLOT(spgeom, install("lines")));
    if (nlines < 1) {
        error("rgeos_project: invalid number of lines");
    }

    int n = LENGTH(d);
    if (n < 1) {
        error("rgeos_interpolate: invalid number of requested points");
    }

    int pc = 0;
    SEXP crd;
    PROTECT(crd = NEW_NUMERIC(n*2)); pc++;

    double x;
    double y;
    SEXP ans;

    // select interpolation function (normalized/unnormalized)
    GEOSGeometry GEOS_DLL *(*interp_fun)(GEOSContextHandle_t,
                                         const GEOSGeometry*,
                                         double);

    if (LOGICAL_POINTER(normalized)[0]) {
        interp_fun = &GEOSInterpolateNormalized_r;
    } else {
        interp_fun = &GEOSInterpolate_r;
    }

    // interpolate points and store result in coord matrix
    for (int i = 0; i < n; i++) {

        dist = NUMERIC_POINTER(d)[i];

        res_geos = (*interp_fun)(GEOShandle, geom, dist);//VG FIXME

        rgeos_Pt2xy(env, res_geos, &x, &y);

        NUMERIC_POINTER(crd)[i] = x;
        NUMERIC_POINTER(crd)[n+i] = y;
    }

    GEOSGeom_destroy_r(GEOShandle, geom);
    GEOSGeom_destroy_r(GEOShandle, res_geos);

    // return coordinates as matrix
    PROTECT(ans = rgeos_formatcrdMat(crd, n)); pc++;

    UNPROTECT(pc);
    return(ans);
}
