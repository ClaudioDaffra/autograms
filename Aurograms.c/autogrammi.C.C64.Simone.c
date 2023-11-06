#if defined( _DEBUG ) && defined( _MSC_VER )
#include <crtdbg.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

//#define IGNORE_PARAMETER( x ) ( void * )( &x )

#define MIN( a, b ) ( ( ( a ) < ( b ) ) ? ( a ) : ( b ) )
#define MAX( a, b ) ( ( ( a ) > ( b ) ) ? ( a ) : ( b ) )

#define ABS( a ) ( ( ( a ) < ( 0 ) ) ? -( a ) : ( a ) )

unsigned long  **table_status;
char **table_conversion;
char  *buffer_string;

void string_concat( char *dst, const char *src )
{
    char       *ptr_dst = dst + strlen( dst );
    const char *ptr_src = src;

    while ( *ptr_src != '\0' )
    {
        *ptr_dst = *ptr_src;

        ptr_dst++;
        ptr_src++;
    }

    *ptr_dst = '\0';
}

unsigned long table_status_size()
{
    return ( unsigned long )( 'z' ) - ( unsigned long )( 'a' ) + 1;
}

void create_table_status()
{
    table_status = ( unsigned long ** )( malloc( table_status_size() * sizeof( unsigned long * ) ) );

    for ( unsigned long i = 0; i < table_status_size(); ++i )
    {
        table_status[ i ] = ( unsigned long * )( malloc( 2 * sizeof( unsigned long ) ) );

        table_status[ i ][ 0 ] = 0;
        table_status[ i ][ 1 ] = 0;
    }
}

void destroy_table_status()
{
    for ( unsigned long i = 0; i < table_status_size(); ++i )
        free( table_status[ i ] );

    free( table_status );
}

unsigned long table_conversion_size()
{
    return 100;
}

void create_table_conversion()
{
    table_conversion = ( char ** )( malloc( table_conversion_size() * sizeof( char * ) ) );

    for ( unsigned long i = 0; i < table_conversion_size(); ++i )
    {
        table_conversion[ i ] = ( char * )( malloc( 20 * sizeof( char ) ) );

        table_conversion[ i ][ 0 ] = '\0';
    }

    string_concat( table_conversion[ 0 ], "zero" );
    string_concat( table_conversion[ 1 ], "one" );
    string_concat( table_conversion[ 2 ], "two" );
    string_concat( table_conversion[ 3 ], "three" );
    string_concat( table_conversion[ 4 ], "four" );
    string_concat( table_conversion[ 5 ], "five" );
    string_concat( table_conversion[ 6 ], "six" );
    string_concat( table_conversion[ 7 ], "seven" );
    string_concat( table_conversion[ 8 ], "eight" );
    string_concat( table_conversion[ 9 ], "nine" );

    string_concat( table_conversion[ 10 ], "ten" );
    string_concat( table_conversion[ 11 ], "eleven" );
    string_concat( table_conversion[ 12 ], "twelve" );
    string_concat( table_conversion[ 13 ], "thirteen" );
    string_concat( table_conversion[ 14 ], "fourteen" );
    string_concat( table_conversion[ 15 ], "fifteen" );
    string_concat( table_conversion[ 16 ], "sixteen" );
    string_concat( table_conversion[ 17 ], "seventeen" );
    string_concat( table_conversion[ 18 ], "eighteen" );
    string_concat( table_conversion[ 19 ], "nineteen" );

    for ( unsigned long i = 20; i <= 29; ++i )
        string_concat( table_conversion[ i ], "twenty" );

    for ( unsigned long i = 30; i <= 39; ++i )
        string_concat( table_conversion[ i ], "thirty" );

    for ( unsigned long i = 40; i <= 49; ++i )
        string_concat( table_conversion[ i ], "forty" );

    for ( unsigned long i = 50; i <= 59; ++i )
        string_concat( table_conversion[ i ], "fifty" );

    for ( unsigned long i = 60; i <= 69; ++i )
        string_concat( table_conversion[ i ], "sixty" );

    for ( unsigned long i = 70; i <= 79; ++i )
        string_concat( table_conversion[ i ], "seventy" );

    for ( unsigned long i = 80; i <= 89; ++i )
        string_concat( table_conversion[ i ], "eighty" );

    for ( unsigned long i = 90; i <= 99; ++i )
        string_concat( table_conversion[ i ], "ninety" );

    for ( unsigned long i = 20; i <= 99; ++i )
        if ( i % 10 != 0 )
        {
            string_concat( table_conversion[ i ], "-" );
            string_concat( table_conversion[ i ], table_conversion[ i % 10 ] );
        }
}

void destroy_table_conversion()
{
    for ( unsigned long i = 0; i < table_conversion_size(); ++i )
        free( table_conversion[ i ] );

    free( table_conversion );
}

unsigned long buffer_string_size()
{
    return 1000;
}

void create_buffer_string()
{
    buffer_string = ( char * )( malloc( buffer_string_size() * sizeof( char ) ) );

    buffer_string[ 0 ] = '\0';
}

void destroy_buffer_string()
{
    free( buffer_string );
}

unsigned long update_buffer_string( const char *sentence )
{
    unsigned long error = 0;

    if ( strlen( buffer_string ) == 0 )
    {
        unsigned long v_sum = 0;

        while ( v_sum == 0 )
        {
            for ( unsigned long i = 0; i < table_status_size(); ++i )
            {
                unsigned long r = rand() % 100 - 80;

                table_status[ i ][ 0 ] = r <= 1 ? 0 : r;
            }

            v_sum = 0;

            for ( unsigned long i = 0; i < table_status_size(); ++i )
                v_sum += table_status[ i ][ 0 ];
        }

        error = table_status_size() * 1000;
    }
    else
    {
        for ( unsigned long i = 0; i < table_status_size(); ++i )
        {
            table_status[ i ][ 1 ] = table_status[ i ][ 0 ];
            table_status[ i ][ 0 ] = 0;
        }

        for ( unsigned long i = 0; i < ( unsigned long )( strlen( buffer_string ) ); ++i )
        {
            char c = buffer_string[ i ];

            if ( c < 'a' || c > 'z' )
                continue;

            unsigned long idx = ( unsigned long )( c ) - ( unsigned long )( 'a' );

            table_status[ idx ][ 0 ]++;
        }

        error = 0;

        for ( unsigned long i = 0; i < table_status_size(); ++i )
            error += ABS( table_status[ i ][ 0 ] - table_status[ i ][ 1 ] );
    }

    unsigned long idx_min = table_status_size() - 1;
    unsigned long idx_max = 0;

    for ( unsigned long i = 0; i < table_status_size(); ++i )
    {
        unsigned long count = table_status[ i ][ 0 ];

        if ( count == 0 )
            continue;

        idx_min = MIN( idx_min, i );
        idx_max = MAX( idx_max, i );
    }

    buffer_string[ 0 ] = '\0';

    string_concat( buffer_string, sentence );

    for ( unsigned long i = 0; i < table_status_size(); ++i )
    {
        unsigned long count = table_status[ i ][ 0 ];

        if ( count == 0 )
            continue;

        if ( i == idx_min )
            string_concat( buffer_string, " " );
        else if ( i == idx_max )
            string_concat( buffer_string, " and " );
        else
            string_concat( buffer_string, ", " );

        char c[ 2 ];

        c[ 0 ] = ( char )( i + ( unsigned long )( 'a' ) );
        c[ 1 ] = '\0';

        string_concat( buffer_string, table_conversion[ count ] );
        string_concat( buffer_string, " " );
        string_concat( buffer_string, c );
        string_concat( buffer_string, count > 1 ? "'s" : "" );
    }

    return error;
}

unsigned long initialize_buffer_string( const char *sentence )
{
    buffer_string[ 0 ] = '\0';

    return update_buffer_string( sentence );
}

void test( const char *sentence, unsigned long max_iterations )
{
    srand( 0 );

    create_table_status();
    create_table_conversion();
    create_buffer_string();

    unsigned long error             = 0;
    unsigned long error_counter     = 0;
    unsigned long error_counter_max = 5;

    // Initialize the solution using random values

    error = initialize_buffer_string( sentence );

    for ( unsigned long i = 0; i < max_iterations; ++i )
    {
        unsigned long error_prev = error;

        // Update the solution

        error = update_buffer_string( sentence );

        if ( error == 0 )
        {
            // The correct solution has been found

            break;
        }
        else
        {
            if ( error >= error_prev )
                error_counter++;

            if ( error_counter > error_counter_max )
            {
                // The algorithm doesn't seem to converge
                // Initialize the solution using random values

                error = initialize_buffer_string( sentence );

                error_counter = 0;
            }
        }
    }

    if ( error == 0 )
        printf( "The solution found is \"%s\".\n", buffer_string );
    else
        printf( "Sorry, no valid solution has been found.\n" );

    destroy_table_status();
    destroy_table_conversion();
    destroy_buffer_string();
}

int main( void )
{
/*
    IGNORE_PARAMETER( argc );
    IGNORE_PARAMETER( argv );

    #if defined( _DEBUG ) && defined( _MSC_VER )
    _CrtSetDbgFlag( _CRTDBG_LEAK_CHECK_DF | _CRTDBG_ALLOC_MEM_DF );
    // _CrtSetBreakAlloc( 0 );
    #endif
*/
    test( "this sentence contains", 50000 );
    putchar(14);
	
    return 0;
}
