#include <iostream>
#include <string>
#include <tuple>
#include <vector>
#include <sstream>
#include <cassert>

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

std::optional<int>
int32_option_to_int_option(const std::optional<int32_t>& n) {
  if (n.has_value()) {
    return std::optional<int>(n.value()) ;
  }
  else {
    return std::optional<int>() ;
  }
}

std::optional<int>
roundtrip_int_option(const std::optional<int>& n) {
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

WALRecoveryMode
wal_recovery_mode_from_int(int n) {
  switch (n) {
  case 0: return WALRecoveryMode::kTolerateCorruptedTailRecords ;
  case 1: return WALRecoveryMode::kAbsoluteConsistency;
  case 2: return WALRecoveryMode::kPointInTimeRecovery;
  case 3: return WALRecoveryMode::kSkipAnyCorruptedRecords;
  default:
    assert(false);
  }
}

} // namespace somecode
