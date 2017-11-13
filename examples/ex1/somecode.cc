#include <string>
#include <vector>
#include <iostream>

#include "somecode.h"

namespace somecode {

std::string
foo(int n) {
  std::string rv = std::to_string(n) ;
  std::cout << "rv = " << rv << std::endl ;
  return rv ;
}

} // namespace somecode
