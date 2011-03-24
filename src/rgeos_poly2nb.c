#include "rgeos.h"

struct ud {
    int count;
    int *ids;
};

void cb(void *item, void *userdata) {
    struct ud *my_UD;
    my_UD = userdata;
    my_UD->ids[my_UD->count] = (int) item;
    my_UD->count++;
}

static struct ud UD;

SEXP rgeos_poly_findInBox(SEXP env, SEXP pls, SEXP as_points) {

    GEOSGeom *bbs;
    int npls, i, j, jj, pc=0;
    GEOSGeom GC, bb;
    SEXP pl, bblist;
    GEOSSTRtree *str;
    int *icard, *ids, *oids;
    int asPTS = LOGICAL_POINTER(as_points)[0];

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    str = (GEOSSTRtree *) GEOSSTRtree_create_r(GEOShandle, (size_t) 10);

    npls = length(pls);
    bbs = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));
    ids = (int *) R_alloc((size_t) npls, sizeof(int));
    UD.ids = (int *) R_alloc((size_t) npls, sizeof(int));
    oids = (int *) R_alloc((size_t) npls, sizeof(int));

    for (i=0; i<npls; i++) {
        ids[i] = i;
        pl = VECTOR_ELT(pls, i);
        if (asPTS) {
              if ((GC = rgeos_Polygons2MP(env, pl)) == NULL) {
                error("rgeos_poly2nb: MP GC[%d] not created", i);
            }
        } else {
              if ((GC = rgeos_Polygons2geospolygon(env, pl)) == NULL) {
                error("rgeos_poly2nb: GC[%d] not created", i);
            }
        }
        if ((bb = GEOSEnvelope_r(GEOShandle, GC)) == NULL) {
            error("rgeos_poly2nb: envelope [%d] not created", i);
        }
        bbs[i] = bb;
        GEOSSTRtree_insert_r(GEOShandle, str, bb, (int *) ids[i]);
    }

    icard = (int *) R_alloc((size_t) npls, sizeof(int));
    PROTECT(bblist = NEW_LIST(npls-1)); pc++;

    for (i=0; i<(npls-1); i++) {
        UD.count = 0;
        GEOSSTRtree_query_r(GEOShandle, str, bbs[i],
            (GEOSQueryCallback *) cb, &UD);
        for (j=0, jj=0; j<UD.count; j++) if (UD.ids[j] > i) jj++;
        icard[i] = jj;
        if (icard[i] > 0) SET_VECTOR_ELT(bblist, i, NEW_INTEGER(icard[i]));

        for (j=0, jj=0; j<UD.count; j++) {
            if (icard[i] > 0 && UD.ids[j] > i) {
                oids[jj] = UD.ids[j] + R_OFFSET;
                jj++;
            }
        }
        R_isort(oids, jj);
        for (j=0; j<jj; j++) {
            INTEGER_POINTER(VECTOR_ELT(bblist, i))[j] = oids[j];
        }
    }

    UNPROTECT(pc);
    return(bblist);
}

SEXP rgeos_binary_STRtree_query(SEXP env, SEXP obj1, SEXP obj2) {

    GEOSGeom *bbs2;
    int nobj1, nobj2, i, j, jj, pc=0;
    GEOSGeom GC, bb;
    SEXP pl, bblist;
    GEOSSTRtree *str;
    int *icard, *ids, *oids;
    char classbuf1[BUFSIZ], classbuf2[BUFSIZ];

    strcpy(classbuf1, CHAR(STRING_ELT(GET_CLASS(VECTOR_ELT(obj1, 0)), 0)));
    strcpy(classbuf2, CHAR(STRING_ELT(GET_CLASS(VECTOR_ELT(obj2, 0)), 0)));

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    str = (GEOSSTRtree *) GEOSSTRtree_create_r(GEOShandle, (size_t) 10);

    nobj1 = length(obj1);
    nobj2 = length(obj2);
    bbs2 = (GEOSGeom *) R_alloc((size_t) nobj2, sizeof(GEOSGeom));
    ids = (int *) R_alloc((size_t) nobj1, sizeof(int));

    UD.ids = (int *) R_alloc((size_t) nobj1, sizeof(int));
    oids = (int *) R_alloc((size_t) nobj1, sizeof(int));

    for (i=0; i<nobj1; i++) {
        ids[i] = i;
        pl = VECTOR_ELT(obj1, i);
        if (!strcmp(classbuf1, "Polygons")) GC = rgeos_Polygons2MP(env, pl);
        else if (!strcmp(classbuf1, "Lines")) GC = rgeos_Lines2MP(env, pl);
        else error("rgeos_binary_STRtree_query: object class %s unknown",
            classbuf1);
        if (GC == NULL) {
            error("rgeos_binary_STRtree_query: MP GC[%d] not created", i);
        }
        if ((bb = GEOSEnvelope_r(GEOShandle, GC)) == NULL) {
            error("rgeos_binary_STRtree_query: envelope [%d] not created", i);
        }
        GEOSSTRtree_insert_r(GEOShandle, str, bb, (int *) ids[i]);
    }

    for (i=0; i<nobj2; i++) {
        pl = VECTOR_ELT(obj2, i);
        if (!strcmp(classbuf2, "Polygons")) GC = rgeos_Polygons2MP(env, pl);
        else if (!strcmp(classbuf2, "Lines")) GC = rgeos_Lines2MP(env, pl);
        else error("rgeos_binary_STRtree_query: object class %s unknown",
            classbuf2);
        if (GC == NULL) {
            error("rgeos_binary_STRtree_query: MP GC[%d] not created", i);
        }
        if ((bb = GEOSEnvelope_r(GEOShandle, GC)) == NULL) {
            error("rgeos_binary_STRtree_query: envelope [%d] not created", i);
        }
        bbs2[i] = bb;
    }

    icard = (int *) R_alloc((size_t) nobj2, sizeof(int));
    PROTECT(bblist = NEW_LIST(nobj2)); pc++;

    for (i=0; i<nobj2; i++) {
        UD.count = 0;
        GEOSSTRtree_query_r(GEOShandle, str, bbs2[i],
            (GEOSQueryCallback *) cb, &UD);

        icard[i] = UD.count;

        if (icard[i] > 0) {
            SET_VECTOR_ELT(bblist, i, NEW_INTEGER(icard[i]));

            for (j=0; j<UD.count; j++) {
                oids[j] = UD.ids[j] + R_OFFSET;
            }
            R_isort(oids, UD.count);
            for (j=0; j<UD.count; j++) {
                INTEGER_POINTER(VECTOR_ELT(bblist, i))[j] = oids[j];
            }
        }
    }

    UNPROTECT(pc);
    return(bblist);
}

SEXP rgeos_unary_STRtree_query(SEXP env, SEXP obj) {

    GEOSGeom *bbs;
    int nobj, i, j, jj, pc=0;
    GEOSGeom GC, bb;
    SEXP pl, bblist;
    GEOSSTRtree *str;
    int *icard, *ids, *oids;
    char classbuf[BUFSIZ];

    strcpy(classbuf, CHAR(STRING_ELT(GET_CLASS(VECTOR_ELT(obj, 0)), 0)));
    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    str = (GEOSSTRtree *) GEOSSTRtree_create_r(GEOShandle, (size_t) 10);

    nobj = length(obj);
    bbs = (GEOSGeom *) R_alloc((size_t) nobj, sizeof(GEOSGeom));
    ids = (int *) R_alloc((size_t) nobj, sizeof(int));
    UD.ids = (int *) R_alloc((size_t) nobj, sizeof(int));
    oids = (int *) R_alloc((size_t) nobj, sizeof(int));

    for (i=0; i<nobj; i++) {
        ids[i] = i;
        pl = VECTOR_ELT(obj, i);
        if (!strcmp(classbuf, "Polygons")) GC = rgeos_Polygons2MP(env, pl);
        else if (!strcmp(classbuf, "Lines")) GC = rgeos_Lines2MP(env, pl);
        else error("rgeos_unary_STRtree_query: object class %s unknown",
            classbuf);
        if (GC == NULL) {
            error("rgeos_unary_STRtree_query: MP GC[%d] not created", i);
        }
        if ((bb = GEOSEnvelope_r(GEOShandle, GC)) == NULL) {
            error("rgeos_unary_STRtree_query: envelope [%d] not created", i);
        }
        bbs[i] = bb;
        GEOSSTRtree_insert_r(GEOShandle, str, bb, (int *) ids[i]);
    }

    icard = (int *) R_alloc((size_t) nobj, sizeof(int));
    PROTECT(bblist = NEW_LIST(nobj-1)); pc++;

    for (i=0; i<(nobj-1); i++) {
        UD.count = 0;
        GEOSSTRtree_query_r(GEOShandle, str, bbs[i],
            (GEOSQueryCallback *) cb, &UD);
        for (j=0, jj=0; j<UD.count; j++) if (UD.ids[j] > i) jj++;
        icard[i] = jj;
        if (icard[i] > 0) {
            SET_VECTOR_ELT(bblist, i, NEW_INTEGER(icard[i]));

            for (j=0, jj=0; j<UD.count; j++) {
                if (UD.ids[j] > i) {
                    oids[jj] = UD.ids[j] + R_OFFSET;
                    jj++;
                }
            }
            R_isort(oids, jj);
            for (j=0; j<jj; j++) {
                INTEGER_POINTER(VECTOR_ELT(bblist, i))[j] = oids[j];
            }
        }
    }

    UNPROTECT(pc);
    return(bblist);
}



