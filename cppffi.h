#include <string>
#include <vector>
#include <tuple>

#ifndef cppffigen_h_included
#define  cppffigen_h_included

/* by construction, [it] should NEVER be NULL */
template<class T>
struct Opt ;

template<class T>
struct OptWrap ;

struct sentinel_GENERIC { } ;
struct sentinel_INT { } ;
struct sentinel_INT32 { } ;
struct sentinel_INT64 { } ;

template <typename T, typename U>
struct sentinel_TUPLE2 { T t ; U u; } ;

template <typename T, typename U, typename V>
struct sentinel_TUPLE3 { T t ; U u; V v; } ;

template <typename T>
struct sentinel_ARRAY { T t; } ;

template <typename T>
struct sentinel_OPTION { T t; } ;

template<class T>
value c2ml_owned_pointer(const T & p) ;

template<class T>
void ml2c_owned_pointer(const value v, T *cv) ;

template<class T>
void ml2c_set_owned_pointer(value v, T cv) ;

template<class T>
value c2ml_opt_owned_pointer(const T *& p) ;

value c2ml(sentinel_INT& unused, int v) ;
value c2ml(sentinel_INT& unused, unsigned int v) ;
value c2ml(sentinel_INT& unused, bool v) ;
value c2ml(sentinel_INT& unused, char v) ;
value c2ml(sentinel_INT& unused, unsigned char v) ;

value c2ml(sentinel_INT32& unused, int32_t v) ;
value c2ml(sentinel_INT32& unused, uint32_t v) ;
value c2ml(sentinel_INT64& unused, int64_t v) ;
value c2ml(sentinel_INT64& unused, uint64_t v) ;

void ml2c(const value v, int *cv) ;
void ml2c(const value v, unsigned int *cv) ;
void ml2c(const value v, bool *cv) ;
void ml2c(const value v, char *cv) ;
void ml2c(const value v, unsigned char *cv) ;

//void ml2c(const value v, int32_t *cv) { *cv = Int32_val(v) ; }
//void ml2c(const value v, uint32_t *cv) { *cv = Int32_val(v) ; }
void ml2c(const value v, int64_t *cv) ;
void ml2c(const value v, uint64_t *cv) ;

value c2ml(sentinel_GENERIC& unused, const std::string& v) ;

void ml2c(const value v, std::string *cv) ;

template<class T, class U, typename ML_T, typename ML_U>
value c2ml(ML_T& unused1, ML_U& unused2, const T& t, const U& u) ;
template<class T, class U, typename ML_T, typename ML_U>
value c2ml(sentinel_TUPLE2<ML_T, ML_U>& unused, const std::tuple<T, U>& v) ;

template<class T, class U>
  void ml2c(const value v, std::tuple<T, U>* cv) ;

template<class T, class U, class V,
	 typename ML_T,
	 typename ML_U,
	 typename ML_V
        >
value c2ml(ML_T& unusedt, ML_U& unusedu, ML_V& unusedv, const T& t, const U& u, const V& v);

template<class T, class U, class V,
	 typename ML_T,
	 typename ML_U,
	 typename ML_V
        >
value c2ml(sentinel_TUPLE3<ML_T, ML_U, ML_V>& unused, const std::tuple<T, U, V>& v) ;

template<class T, class U, class V>
  void ml2c(const value v, std::tuple<T, U, V>* cv) ;

template<class T, typename ML_T>
value c2ml(sentinel_ARRAY<ML_T> unused, const std::vector<T>& v) ;

template<typename T>
void ml2c(const value v, std::vector<T>* cv) ;

template<class T, typename ML_T>
value c2ml_opt(ML_T& unused, const T *&p) ;

template <class T, typename ML_T>
value c2ml(sentinel_OPTION<ML_T> unused, const Opt<T> *ssp) ;

#endif // cppffigen_h_included
