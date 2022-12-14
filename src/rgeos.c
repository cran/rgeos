#include <math.h>
#include "rgeos.h"
//static void rgeos_finish_handle(SEXP ptr);
//static GEOSContextHandle_t sr;

SEXP rgeos_GEOSversion(SEXP runtime) {

    int rt = LOGICAL_POINTER(runtime)[0];

    SEXP ans;
    PROTECT(ans = NEW_CHARACTER(1));
    if (rt) {
        SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(GEOSversion()));
    } else {
        SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(GEOS_CAPI_VERSION));
    }
    UNPROTECT(1);
    return(ans);
}

SEXP rgeos_sp_linkingTo_version(void) {
    return(SP_PREFIX(sp_linkingTo_version)());
}

// from line 79, postgis/liblwgeom/lwgeom_geos.c
/* Free any non-null GEOSGeometry* pointers passed as arguments *
 * Called by GEOS_FREE, which populates 'count' */
/*static void geos_destroy(size_t count, ...) {
	va_list ap;
	va_start(ap, count);
	while (count--)
	{
		GEOSGeometry* g = va_arg(ap, GEOSGeometry*);
		if (g)
		{
			GEOSGeom_destroy(g);
		}
	}
}*/

char errbuf[BUFSIZ];
int errbuf_set;

char* get_errbuf(void) {

    if (errbuf_set) return(errbuf);
    else return(NULL);

}

void unset_errbuf(void) {

    if (errbuf_set) {
        errbuf[0] = '\0';
        errbuf_set = 0;
    }

}


int is_errbuf_set(void) {

    return (errbuf_set);

}


static void __errorHandler(const char *fmt, ...) {

    char *p;
    va_list ap;
    // from line 56, postgis/liblwgeom/lwgeom_geos.c
/*    do {
        geos_destroy((sizeof((void*[])...)/sizeof(void*)),
            ...);
    } while (0);*/
    va_start(ap, fmt);
    unset_errbuf();
    vsnprintf(errbuf, BUFSIZ-1, fmt, ap);
    va_end(ap);
    p = errbuf + strlen(errbuf) - 1;
    if(strlen(errbuf) > 0 && *p == '\n') *p = '\0';
    errbuf_set = 1;
//Rprintf("ptr: %d\n", (int) sr);
//    finishGEOS_r(sr);

//    error(errbuf);

    return;
}

static void __warningHandler(const char *fmt, ...) {

    char buf[BUFSIZ], *p;
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';

    warning(buf);
    
    return;
}

GEOSContextHandle_t getContextHandle(SEXP env) {

    SEXP ptr = findVarInFrame(env, install("GEOSptr"));
    GEOSContextHandle_t r = R_ExternalPtrAddr(ptr);

    return(r);
}

SEXP rgeos_Init(void) {

    GEOSContextHandle_t r = initGEOS_r((GEOSMessageHandler) __warningHandler, (GEOSMessageHandler) __errorHandler);
// rchk replace mkChar with install RSB 180602
//    sr = initGEOS_r((GEOSMessageHandler) __warningHandler, (GEOSMessageHandler) __errorHandler);
    SEXP sxpHandle = R_MakeExternalPtr((void *) r, install("GEOSContextHandle"), R_NilValue);
//    SEXP sxpHandle = R_MakeExternalPtr((void *) sr, install("GEOSContextHandle"), R_NilValue);
//    R_RegisterCFinalizerEx(sxpHandle, rgeos_finish_handle, TRUE);
 
    return(sxpHandle);
}

//static void rgeos_finish_handle(SEXP ptr) {
//
//    if(!R_ExternalPtrAddr(ptr)) return;
//    R_ClearExternalPtr(ptr);
//}


SEXP rgeos_finish(SEXP env) {

    GEOSContextHandle_t r = getContextHandle(env);
    finishGEOS_r(r);

    SEXP sxpHandle = findVarInFrame(env, install("GEOSptr"));
//    rgeos_finish_handle(sxpHandle);
    if(!R_ExternalPtrAddr(sxpHandle)) return(R_NilValue);
    R_ClearExternalPtr(sxpHandle);

    return(R_NilValue);
}



double getScale(SEXP env) {

    return( NUMERIC_POINTER( findVarInFrame(env, install("scale")) )[0] );
}

double rgeos_round(double val) {
    
    return( java_math_round(val) );
}


double makePrecise(double val, double scale) {
    
    return( rgeos_round(val*scale)/scale );
}

// Symmetric Rounding Algorithm  - equivalent to C99 round()
double sym_round(double val) {
    double n;
    double f = fabs(modf(val, &n));
    if (val >= 0) {
        if (f < 0.5) {
            return( floor(val) );
        } else if (f > 0.5) {
            return( ceil(val) );
        } else {
            return( n + 1.0 );
        }
    } else {
        if (f < 0.5) {
            return( ceil(val) );
        } else if (f > 0.5) {
            return( floor(val) );
        } else {
            return( n - 1.0 );
        }
    }
}


// Asymmetric Rounding Algorithm  - equivalent to Java Math.round()
double java_math_round(double val) {
    double n;
    double f = fabs(modf(val, &n));

    if (val >= 0) {
        if (f < 0.5) {
            return( floor(val) );
        } else if (f > 0.5) {
            return( ceil(val) );
        } else {
            return( n + 1.0 );
        }
    } else {
        if (f < 0.5) {
            return( ceil(val) );
        } else if (f > 0.5) {
            return( floor(val) );
        } else {
            return( n );
        }
    }
} 

// Implementation of rint() 
double rint_vc(double val) {
    double n;
    double f=fabs(modf(val,&n));
    if (val>=0) {
        if (f<0.5) {
            return( floor(val) );
        } else if (f>0.5) {
            return( ceil(val) );
        } else {
            return( (floor(n/2)==n/2)?n:n+1.0 );
        }
    } else {
        if (f<0.5) {
            return( ceil(val) );
        } else if (f>0.5) {
            return( floor(val) );
        } else {
            return(( floor(n/2)==n/2)?n:n-1.0 );
        }
    }
}
