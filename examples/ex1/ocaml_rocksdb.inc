/*
   Copyright (c) 2012,2015 Monty Program Ab

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; version 2 of the License.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */

/* C++ system header files */
#include <string>

/* RocksDB header files */
#include "rocksdb/comparator.h"
#include "rocksdb/slice.h"

namespace myrocks {

/*
  The keys are in form: {index_number} {mem-comparable-key}

  (todo: knowledge about this format is shared between this class and
   Rdb_key_def)
*/
class Rdb_pk_comparator : public rocksdb::Comparator
{
 public:
  Rdb_pk_comparator(const Rdb_pk_comparator&) = delete;
  Rdb_pk_comparator& operator=(const Rdb_pk_comparator&) = delete;
  Rdb_pk_comparator() = default;

  static int bytewise_compare(const rocksdb::Slice& a, const rocksdb::Slice& b)
  {
    const size_t a_size= a.size();
    const size_t b_size= b.size();
    const size_t len= (a_size < b_size) ? a_size : b_size;
    int res;

    if ((res= memcmp(a.data(), b.data(), len)))
      return res;

    /* Ok, res== 0 */
    if (a_size != b_size)
    {
      return a_size < b_size? -1 : 1;
    }
    return 0;
  }

  /* Override virtual methods of interest */

  int Compare(const rocksdb::Slice& a, const rocksdb::Slice& b) const override
  {
    return bytewise_compare(a,b);
  }

  const char* Name() const override { return "RocksDB_SE_v3.10"; }

  //TODO: advanced funcs:
  // - FindShortestSeparator
  // - FindShortSuccessor

  // for now, do-nothing implementations:
  void FindShortestSeparator(std::string* start,
                             const rocksdb::Slice& limit) const override {}
  void FindShortSuccessor(std::string* key) const override {}
};
 class Rdb_rev_comparator : public rocksdb::Comparator
{
 public:
  Rdb_rev_comparator(const Rdb_rev_comparator&) = delete;
  Rdb_rev_comparator& operator=(const Rdb_rev_comparator&) = delete;
  Rdb_rev_comparator() = default;

  static int bytewise_compare(const rocksdb::Slice& a, const rocksdb::Slice& b)
  {
    return -Rdb_pk_comparator::bytewise_compare(a, b);
  }

  int Compare(const rocksdb::Slice& a, const rocksdb::Slice& b) const override
  {
    return -Rdb_pk_comparator::bytewise_compare(a, b);
  }
  const char* Name() const override { return "rev:RocksDB_SE_v3.10"; }
  void FindShortestSeparator(std::string* start,
                             const rocksdb::Slice& limit) const override {}
  void FindShortSuccessor(std::string* key) const override {}
};



}  // namespace myrocks

#define dboptions_t_DEFINED
struct dboptions_t {
  bool create_if_missing ;
  bool create_missing_column_families ;
  bool error_if_exists ;
  bool paranoid_checks ;
  /*InfoLogLevel*/
  unsigned char info_log_level ;
  int max_open_files ;
  int max_file_opening_threads ;
  uint64_t max_total_wal_size ;
  bool use_fsync ;
  /*  std::vector<DbPath> db_paths; */
  std::string db_log_dir ;
  std::string wal_dir ;
  uint64_t delete_obsolete_files_period_micros ;
  int max_background_jobs ;
  int base_background_compactions ;
  int max_background_compactions ;
  uint32_t max_subcompactions ;
  int max_background_flushes ;
  size_t max_log_file_size ;
  size_t log_file_time_to_roll ;
  size_t keep_log_file_num ;
  size_t recycle_log_file_num ;
  uint64_t max_manifest_file_size ;
  int table_cache_numshardbits ;
  uint64_t _WAL_ttl_seconds ;
  uint64_t _WAL_size_limit_MB ;
  size_t manifest_preallocation_size ;
  bool allow_mmap_reads ;
  bool allow_mmap_writes ;
  bool use_direct_reads ;
  bool use_direct_io_for_flush_and_compaction ;
  bool allow_fallocate ;
  bool is_fd_close_on_exec ;
  bool skip_log_error_on_recovery ;
  unsigned int stats_dump_period_sec ;
  bool advise_random_on_open ;
  size_t db_write_buffer_size ;
  /* AccessHint */
  int access_hint_on_compaction_start ;
  bool new_table_reader_for_compaction_inputs ;
  size_t compaction_readahead_size ;
  size_t random_access_max_buffer_size ;
  size_t writable_file_max_buffer_size ;
  bool use_adaptive_mutex ;
  uint64_t bytes_per_sync ;
  uint64_t wal_bytes_per_sync ;
  bool enable_thread_tracking ;
  uint64_t delayed_write_rate ;
  bool enable_pipelined_write ;
  bool allow_concurrent_memtable_write ;
  bool enable_write_thread_adaptive_yield ;
  uint64_t write_thread_max_yield_usec ;
  uint64_t write_thread_slow_yield_usec ;
  bool skip_stats_update_on_db_open ;
  rocksdb::WALRecoveryMode wal_recovery_mode ;
  bool allow_2pc ;
  bool fail_if_options_file_error ;
  bool dump_malloc_stats ;
  bool avoid_flush_during_recovery ;
  bool avoid_flush_during_shutdown ;
  bool allow_ingest_behind ;
  bool concurrent_prepare ;
  bool manual_wal_flush ;
  dboptions_t() {}
  dboptions_t(const rocksdb::DBOptions &opts) : 
  create_if_missing(opts.create_if_missing) ,
    create_missing_column_families(opts.create_missing_column_families) ,
    error_if_exists(opts.error_if_exists) ,
    paranoid_checks(opts.paranoid_checks) ,
  /*InfoLogLevel*/
    info_log_level(opts.info_log_level) ,
    max_open_files(opts.max_open_files) ,
    max_file_opening_threads(opts.max_file_opening_threads) ,
    max_total_wal_size(opts.max_total_wal_size) ,
    use_fsync(opts.use_fsync) ,
  /*  std::vector<DbPath> db_paths; */
    db_log_dir(opts.db_log_dir) ,
    wal_dir(opts.wal_dir) ,
    delete_obsolete_files_period_micros(opts.delete_obsolete_files_period_micros) ,
    max_background_jobs(opts.max_background_jobs) ,
    base_background_compactions(opts.base_background_compactions) ,
    max_background_compactions(opts.max_background_compactions) ,
    max_subcompactions(opts.max_subcompactions) ,
    max_background_flushes(opts.max_background_flushes) ,
    max_log_file_size(opts.max_log_file_size) ,
    log_file_time_to_roll(opts.log_file_time_to_roll) ,
    keep_log_file_num(opts.keep_log_file_num) ,
    recycle_log_file_num(opts.recycle_log_file_num) ,
    max_manifest_file_size(opts.max_manifest_file_size) ,
    table_cache_numshardbits(opts.table_cache_numshardbits) ,
    _WAL_ttl_seconds(opts.WAL_ttl_seconds) ,
    _WAL_size_limit_MB(opts.WAL_size_limit_MB) ,
    manifest_preallocation_size(opts.manifest_preallocation_size) ,
    allow_mmap_reads(opts.allow_mmap_reads) ,
    allow_mmap_writes(opts.allow_mmap_writes) ,
    use_direct_reads(opts.use_direct_reads) ,
    use_direct_io_for_flush_and_compaction(opts.use_direct_io_for_flush_and_compaction) ,
    allow_fallocate(opts.allow_fallocate) ,
    is_fd_close_on_exec(opts.is_fd_close_on_exec) ,
    skip_log_error_on_recovery(opts.skip_log_error_on_recovery) ,
    stats_dump_period_sec(opts.stats_dump_period_sec) ,
    advise_random_on_open(opts.advise_random_on_open) ,
    db_write_buffer_size(opts.db_write_buffer_size) ,
  /* AccessHint */
    access_hint_on_compaction_start(opts.access_hint_on_compaction_start) ,
    new_table_reader_for_compaction_inputs(opts.new_table_reader_for_compaction_inputs) ,
    compaction_readahead_size(opts.compaction_readahead_size) ,
    random_access_max_buffer_size(opts.random_access_max_buffer_size) ,
    writable_file_max_buffer_size(opts.writable_file_max_buffer_size) ,
    use_adaptive_mutex(opts.use_adaptive_mutex) ,
    bytes_per_sync(opts.bytes_per_sync) ,
    wal_bytes_per_sync(opts.wal_bytes_per_sync) ,
    enable_thread_tracking(opts.enable_thread_tracking) ,
    delayed_write_rate(opts.delayed_write_rate) ,
    enable_pipelined_write(opts.enable_pipelined_write) ,
    allow_concurrent_memtable_write(opts.allow_concurrent_memtable_write) ,
    enable_write_thread_adaptive_yield(opts.enable_write_thread_adaptive_yield) ,
    write_thread_max_yield_usec(opts.write_thread_max_yield_usec) ,
    write_thread_slow_yield_usec(opts.write_thread_slow_yield_usec) ,
    skip_stats_update_on_db_open(opts.skip_stats_update_on_db_open) ,
  /* WALRecoveryMode */
    wal_recovery_mode(opts.wal_recovery_mode) ,
    allow_2pc(opts.allow_2pc) ,
    fail_if_options_file_error(opts.fail_if_options_file_error) ,
    dump_malloc_stats(opts.dump_malloc_stats) ,
    avoid_flush_during_recovery(opts.avoid_flush_during_recovery) ,
    avoid_flush_during_shutdown(opts.avoid_flush_during_shutdown) ,
    allow_ingest_behind(opts.allow_ingest_behind) ,
    concurrent_prepare(opts.concurrent_prepare) ,
    manual_wal_flush(opts.manual_wal_flush)
{}
} ;



