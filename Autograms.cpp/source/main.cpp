#if defined( _DEBUG )
#include <crtdbg.h>
#endif

#include <exception>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

#include "core/autogram_solver_helper.hpp"

#include "utils/clock.hpp"

int main( int argc, char **argv )
{
    #if defined( _DEBUG )
    _CrtSetDbgFlag( _CRTDBG_LEAK_CHECK_DF | _CRTDBG_ALLOC_MEM_DF );
    // _CrtSetBreakAlloc( 0 );
    #endif

    try
    {
        std::string sentence = "this sentence contains";

        int max_iterations = 2000000;

        int result_type = autogram_solver::options::none;

        if ( argc > 1 )
        {
            sentence = std::string( argv[ 1 ] );

            if ( ( argc - 2 ) % 2 == 0 )
            {
                for ( int i = 0; i < ( argc - 2 ) / 2; ++i )
                {
                    int idx1 = i * 2 + 2;
                    int idx2 = i * 2 + 3;

                    if ( std::string( argv[ idx1 ] ) == "-i" )
                        max_iterations = std::stoi( argv[ idx2 ] );

                    if ( std::string( argv[ idx1 ] ) == "-o" )
                        result_type = std::string( argv[ idx2 ] ) == "force-pangram"
                                      ? autogram_solver::options::force_pangram
                                      : autogram_solver::options::none;
                }
            }
        }

        clock::tic();

        std::cout << "Computing..." << std::endl;

        autogram_solver_helper solver;

        std::string result = solver.compute( sentence, max_iterations, result_type );

        if ( result.empty() )
        {
            std::cout << "Sorry, no valid solution has been found." << std::endl;
        }
        else
        {
            std::cout << "The solution found is \"" << result << "\"." << std::endl;

            std::stringstream buf;

            buf << std::setiosflags( std::stringstream::fixed )
                << std::setiosflags( std::stringstream::showpoint )
                << std::setprecision( 4 );

            buf << "Done in " << clock::toc() << " seconds.";

            std::cout << buf.str() << std::endl;
        }
    }
    catch ( std::exception &e )
    {
        std::cout << "+++ ERROR +++ (" << e.what() << ")" << std::endl;
    }

    return 0;
}
