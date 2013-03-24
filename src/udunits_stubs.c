/*
 * An OCaml wrapper for the UDUNITS-2 C library.
 * by Hezekiah M. Carty
 *
 */

/* The "usual" OCaml includes */
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/custom.h>

/* UDUNITS-2 API include */
#include <udunits2.h>

/* For debugging - we want to have access to printf, stderr and such */
#include <stdio.h>
#include <string.h>

// Allow for ridiculously long exception strings.
#define MAX_EXCEPTION_MESSAGE_LENGTH 10000

#define UD_ut_system_val(val) (* ((ut_system **) Data_custom_val(val)))
#define UD_ut_unit_val(val) (* ((ut_unit **) Data_custom_val(val)))
#define UD_cv_converter_val(val) (* ((cv_converter **) Data_custom_val(val)))

// Generic status checker and exception raiser
#define CHECK_STATUS(x) \
    if ( (x) == NULL ) { \
        caml_raise_with_arg( *caml_named_value("ut status exception"), Val_int( ut_get_status() ) ); \
    }

// OCaml handlers to free values when they are GC'd
#define MAKE_FINALIZE(t, conv, t_free) \
void finalize_##t(value ml_##t) { \
    t *c_##t; \
    c_##t = conv(ml_##t); \
    t_free(c_##t); \
    return; \
}

MAKE_FINALIZE(ut_system, UD_ut_system_val, ut_free_system)
MAKE_FINALIZE(ut_unit, UD_ut_unit_val, ut_free)
MAKE_FINALIZE(cv_converter, UD_cv_converter_val, cv_free)

// Definition for custom OCaml handler functions
#define MAKE_CUSTOM_OPS(t) \
static struct custom_operations t##_custom_ops = { \
    identifier: #t, \
    finalize: finalize_##t, \
    compare: custom_compare_default, \
    hash: custom_hash_default, \
    serialize: custom_serialize_default, \
    deserialize: custom_deserialize_default \
};

MAKE_CUSTOM_OPS(ut_system)
MAKE_CUSTOM_OPS(ut_unit)
MAKE_CUSTOM_OPS(cv_converter)

// C -> OCaml conversion
#define MAKE_VAL(t) \
value Val_##t(t *x) { \
    t **store; \
    value ret; \
    ret = caml_alloc_custom(&t##_custom_ops, sizeof(x), 0, 1); \
    store = Data_custom_val(ret); \
    *store = x; \
    return ret; \
}

MAKE_VAL(ut_system)
MAKE_VAL(ut_unit)
MAKE_VAL(cv_converter)

/*
 * Initialization
 */

// OCaml-friendly initialization
value ml_init( value unit ) {
    CAMLparam1( unit );

    ut_set_error_message_handler( ut_ignore );

    CAMLreturn( Val_unit );
}

/*
 * Status checks
 */

value ml_ut_get_status( value unit ) {
    CAMLparam1( unit );

    CAMLreturn( Val_int( ut_get_status() ) );
}

/*
 * Dealing with unit systems
 */

value ml_ut_read_xml( value path ) {
    CAMLparam1( path );
    ut_system *s;
    s = ut_read_xml( String_val( path ) );
    CHECK_STATUS( s )
    CAMLreturn( Val_ut_system( s ) );
}
value ml_ut_read_xml_default( value _unit ) {
    CAMLparam1( _unit );
    ut_system *s;
    s = ut_read_xml( NULL );
    CHECK_STATUS( s )
    CAMLreturn( Val_ut_system( s ) );
}

/*
 * Unit conversion
 */

value ml_ut_are_convertible( value from, value to ) {
    CAMLparam2( from, to );

    int result;
    result = ut_are_convertible( UD_ut_unit_val( from ), UD_ut_unit_val( to ) );

    CAMLreturn( Val_bool( result ) );
}

value ml_ut_get_converter( value from, value to ) {
    CAMLparam2( from, to );

    cv_converter *c;

    c = ut_get_converter( UD_ut_unit_val( from ), UD_ut_unit_val( to ) );
    CHECK_STATUS( c )
    CAMLreturn( Val_cv_converter( c ) );
}

value ml_cv_convert_double( value converter, value x ) {
    CAMLparam2( converter, x );

    double result;
    result = cv_convert_double( UD_cv_converter_val( converter ), Double_val( x ) );

    CAMLreturn( caml_copy_double( result ) );
}

value ml_cv_convert_bigarray( value converter, value src, value dest ) {
    CAMLparam3( converter, src, dest );

    int n;
    n = Bigarray_val( dest )->dim[0];

    if ( n > Bigarray_val( src )->dim[0] ) {
        caml_raise_with_arg( *caml_named_value( "ut status exception" ), Val_int( UT_BAD_ARG ) );
    }

    if ( (Bigarray_val( src )->flags & BIGARRAY_KIND_MASK) == BIGARRAY_FLOAT32 ) {
        cv_convert_floats( UD_cv_converter_val( converter ), Data_bigarray_val( src ), n, Data_bigarray_val( dest ) );
    }
    else if ( (Bigarray_val( src )->flags & BIGARRAY_KIND_MASK) == BIGARRAY_FLOAT64 ) {
        cv_convert_doubles( UD_cv_converter_val( converter ), Data_bigarray_val( src ), n, Data_bigarray_val( dest ) );
    }
    else {
        caml_raise_with_arg( *caml_named_value( "ut status exception" ), Val_int( UT_BAD_ARG ) );
    }

    CAMLreturn( Val_unit );
}

value ml_cv_convert_array( value converter, value src, value dest ) {
    CAMLparam3( converter, src, dest );

    size_t n;
    n = Wosize_val( dest ) / Double_wosize;

    if ( n > ( Wosize_val( src ) / Double_wosize ) ) {
        caml_raise_with_arg( *caml_named_value( "ut status exception" ), Val_int( UT_BAD_ARG ) );
    }

    cv_convert_doubles( UD_cv_converter_val( converter ), (double *)src, n, (double *)dest );

    CAMLreturn( Val_unit );
}

/*
 * Unit definitions
 */

value ml_ut_parse( value system, value s, value encoding ) {
    CAMLparam3( system, s, encoding );

    ut_unit *u;
    u = ut_parse( UD_ut_system_val( system ), String_val( s ), Int_val( encoding ) );
    CHECK_STATUS( u )

    CAMLreturn( Val_ut_unit( u ) );
}

value ml_ut_get_name( value u, value encoding ) {
    CAMLparam2( u, encoding );

    const char *result;
    result = ut_get_name( UD_ut_unit_val( u ), Int_val( encoding ) );
    CHECK_STATUS( result )

    CAMLreturn( caml_copy_string( result ) );
}

value ml_ut_get_symbol( value u, value encoding ) {
    CAMLparam2( u, encoding );

    const char *result;
    result = ut_get_symbol( UD_ut_unit_val( u ), Int_val( encoding ) );
    CHECK_STATUS( result )

    CAMLreturn( caml_copy_string( result ) );
}

