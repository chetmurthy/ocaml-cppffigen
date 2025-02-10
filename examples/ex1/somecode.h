#include <iostream>
#include <string>
#include <tuple>
#include <vector>

#ifndef somecode_h_included
#define somecode_h_included

namespace somecode {

std::string
  foo(int n) ;

int32_t
int_to_int32(int n) ;

int
int32_to_int(int32_t n) ;

std::tuple< std::string, int >
  bar(std::string s, int n);

std::string
size_t_to_string(size_t n);

} // namespace somecode

#endif
