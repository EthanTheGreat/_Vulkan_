// Copyright 2016 Intel Corporation All Rights Reserved
// 
// Intel makes no representations about the suitability of this software for any purpose.
// THIS SOFTWARE IS PROVIDED ""AS IS."" INTEL SPECIFICALLY DISCLAIMS ALL WARRANTIES,
// EXPRESS OR IMPLIED, AND ALL LIABILITY, INCLUDING CONSEQUENTIAL AND OTHER INDIRECT DAMAGES,
// FOR THE USE OF THIS SOFTWARE, INCLUDING LIABILITY FOR INFRINGEMENT OF ANY PROPRIETARY
// RIGHTS, AND INCLUDING THE WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
// Intel does not assume any responsibility for any errors which may appear in this software
// nor any responsibility to update it.

#include <fstream>
#include <iostream>
#include "Tools.h"

namespace ApiWithoutSecrets {

  namespace Tools {

    std::vector<char> GetBinaryFileContents( std::string const &filename ) {

      std::ifstream file( filename, std::ios::binary );
      if( file.fail() ) {
        std::cout << "Could not open \"" << filename << "\" file!" << std::endl;
        return std::vector<char>();
      }

      std::streampos begin, end;
      begin = file.tellg();
      file.seekg( 0, std::ios::end );
      end = file.tellg();

      std::vector<char> result( static_cast<size_t>(end - begin) );
      file.seekg( 0, std::ios::beg );
      file.read( &result[0], end - begin );
      file.close();

      return result;
    }

  } // namespace Tools

} // namespace ApiWithoutSecrets