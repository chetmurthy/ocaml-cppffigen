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

enum class WALRecoveryMode : char {
  kTolerateCorruptedTailRecords = 0x00,
  kAbsoluteConsistency = 0x01,
  kPointInTimeRecovery = 0x02,
  kSkipAnyCorruptedRecords = 0x03,
};

WALRecoveryMode
wal_recovery_mode_from_int(int n);

} // namespace somecode

#endif
