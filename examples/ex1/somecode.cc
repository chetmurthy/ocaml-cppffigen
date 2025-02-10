#include <iostream>
#include <string>
#include <tuple>
#include <vector>
#include <sstream>

#include "somecode.h"

namespace somecode {

std::string
foo(int n) {
  std::string rv = std::to_string(n) ;
  return rv ;
}

int32_t
int_to_int32(int n) {
  return n ;
}

int
int32_to_int(int32_t n) {
  return n ;
}

std::tuple< std::string, int >
bar(std::string s, int n) {
  return std::tuple< std::string, int>{ s, n } ;
}

std::string
size_t_to_string(size_t n) {
  //  std::string rv = std::to_string(n) ;

  std::stringstream sstream;
  sstream << std::hex << n;
  std::string rv = sstream.str();

  return rv ;
}

} // namespace somecode
