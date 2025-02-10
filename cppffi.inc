/* these definitions probably need to go, but I'll leave them here
until I have more tests/examples */

/* by construction, [it] should NEVER be NULL */
template<class T>
struct Opt {
  T *it ;
  explicit Opt (T *p) : it(p) {}
} ;

template<class T>
struct OptWrap {
public:
  explicit OptWrap(T *p) : opt_(p) {
    if (NULL == p) { p_ = NULL; }
    else { p_ = &opt_ ; }
  }
  Opt<T> *p() { return p_ ; }

private:
  Opt<T> opt_;
  Opt<T> *p_ ;
} ;

template<class T>
value c2ml_owned_pointer(const T & p) {
  assert (NULL != p) ;
  value _v1;
  _v1 = caml_alloc_small((sizeof(T) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((T *) Bp_val(_v1)) = p;
  return _v1;
}

template<class T>
void ml2c_owned_pointer(const value v, T *cv) {
  *cv = *((T *) Bp_val(v));
}

template<class T>
void ml2c_set_owned_pointer(value v, T cv) {
  *((T *) Bp_val(v)) = cv ;
}

template<class T>
value c2ml_opt_owned_pointer(const T *& p) {
    OptWrap<T> w(p) ;
    return c2ml(w.p()) ;
}

value c2ml(sentinel_INT& unused, int v) { return Val_int(v) ; }
value c2ml(sentinel_INT& unused, unsigned int v) { return Val_int(v) ; }
value c2ml(sentinel_INT& unused, bool v) { return Val_bool(v) ; }
value c2ml(sentinel_INT& unused, char v) { return Val_int((int)v) ; }
value c2ml(sentinel_INT& unused, unsigned char v) { return Val_int((int)v) ; }

value c2ml(sentinel_INT32& unused, int32_t v) { return caml_copy_int32(v) ; }
value c2ml(sentinel_INT32& unused, uint32_t v) { return caml_copy_int32(v) ; }
value c2ml(sentinel_INT64& unused, int64_t v) { return caml_copy_int64(v) ; }
value c2ml(sentinel_INT64& unused, uint64_t v) { return caml_copy_int64(v) ; }

void ml2c(sentinel_INT& unused, const value v, int *cv) { *cv = Int_val(v) ; }
void ml2c(sentinel_INT& unused, const value v, unsigned int *cv) { *cv = Int_val(v) ; }
void ml2c(sentinel_INT& unused, const value v, bool *cv) { *cv = Bool_val(v) ; }
void ml2c(sentinel_INT& unused, const value v, char *cv) { *cv = (char)Int_val(v) ; }
void ml2c(sentinel_INT& unused, const value v, unsigned char *cv) { *cv = (char)Int_val(v) ; }

void ml2c(sentinel_INT32& unused, const value v, int32_t *cv) { *cv = Int32_val(v) ; }
void ml2c(sentinel_INT32& unused, const value v, uint32_t *cv) { *cv = Int32_val(v) ; }
void ml2c(sentinel_INT64& unused, const value v, int64_t *cv) { *cv = Int64_val(v) ; }
void ml2c(sentinel_INT64& unused, const value v, uint64_t *cv) { *cv = Int64_val(v) ; }

value c2ml(sentinel_GENERIC& unused, const std::string& v) {
  uint32_t size = v.size() ;
  value res = caml_alloc_string(size);
  memmove((void *)String_val(res), (const void *)v.c_str(), size);
  return res ;
}

void ml2c(sentinel_GENERIC& unused, const value v, std::string *cv) {
  int size = caml_string_length(v) ;
  *cv = std::string(String_val(v), size) ;
}

template<class T, class U, typename ML_T, typename ML_U>
value c2ml(ML_T& unused1, ML_U& unused2, const T& t, const U& u) {
  CAMLparam0() ;
  CAMLlocal1(mlv) ;
  mlv = caml_alloc_tuple(2) ;
  Store_field(mlv, 0, c2ml(unused1, t)) ;
  Store_field(mlv, 1, c2ml(unused2, u)) ;
  CAMLreturn(mlv) ;
}
template<class T, class U, typename ML_T, typename ML_U>
value c2ml(sentinel_TUPLE2<ML_T, ML_U>& unused, const std::tuple<T, U>& v) {
  return c2ml(unused.t, unused.u, std::get<0>(v), std::get<1>(v)) ;
}

template<class T, class U, typename ML_T, typename ML_U>
void ml2c(sentinel_TUPLE2<ML_T, ML_U>& unused, const value v, std::tuple<T, U>* cv) {
  ML_T t;
  ml2c(t, Field(v, 0), &(std::get<0>(*cv))) ;
  ML_U u;
  ml2c(u, Field(v, 1), &(std::get<1>(*cv))) ;
}

template<class T, class U, class V,
	 typename ML_T,
	 typename ML_U,
	 typename ML_V
        >
value c2ml(ML_T& unusedt, ML_U& unusedu, ML_V& unusedv, const T& t, const U& u, const V& v) {
  CAMLparam0() ;
  CAMLlocal1(mlv) ;
  mlv = caml_alloc_tuple(3) ;
  Store_field(mlv, 0, c2ml(unusedt, t)) ;
  Store_field(mlv, 1, c2ml(unusedu, u)) ;
  Store_field(mlv, 2, c2ml(unusedv, v)) ;
  CAMLreturn(mlv) ;
}
template<class T, class U, class V,
	 typename ML_T,
	 typename ML_U,
	 typename ML_V
        >
value c2ml(sentinel_TUPLE3<ML_T, ML_U, ML_V>& unused, const std::tuple<T, U, V>& v) {
  return c2ml(unused.t, unused.u, unused.v, std::get<0>(v), std::get<1>(v), std::get<2>(v)) ;
}

template<class T, class U, class V,
	 typename ML_T,
	 typename ML_U,
	 typename ML_V
        >
void ml2c(sentinel_TUPLE3<ML_T, ML_U, ML_V>& unused, const value v, std::tuple<T, U, V>* cv) {
  ML_T u_t;
  ml2c(u_t, Field(v, 0), &(std::get<0>(*cv))) ;
  ML_U u_u;
  ml2c(u_u, Field(v, 1), &(std::get<1>(*cv))) ;
  ML_V u_v;
  ml2c(u_v, Field(v, 2), &(std::get<2>(*cv))) ;
}

template<class T, typename ML_T>
value c2ml(sentinel_ARRAY<ML_T> unused, const std::vector<T>& v) {
  CAMLparam0() ;
  CAMLlocal1(mlv) ;
  mlv = caml_alloc(v.size(), 0) ;
  ML_T unused1;
  for(int i = 0 ; i < v.size(); i++) {
    Store_field(mlv, i, c2ml(unused1, v[i])) ;
  }
  CAMLreturn(mlv) ;
}

template<typename T, typename ML_T>
void ml2c(sentinel_ARRAY<ML_T> unused, const value v, std::vector<T>* cv) {
  ML_T t ;
  int n = caml_array_length(v) ;
  cv->clear();
  cv->resize(n) ;
  for(int i = 0 ; i < n ; i++) {
    T tmp ;
    ml2c(t, Field(v, i), &tmp) ;
    (*cv)[i] = tmp ;
  }
}

template<class T, typename ML_T>
value c2ml_opt(ML_T& unused, const T *&p) {
    CAMLparam0 () ;
  if (NULL == p) {
    CAMLreturn (Val_int(0)) ;
  }
  else {
    CAMLlocal1 (v) ;
    v = caml_alloc_small(1, 0) ;
    Field(v, 0) = c2ml(unused, *p) ;
    CAMLreturn(v) ;
  }
}

template <class T, typename ML_T>
value c2ml(sentinel_OPTION<ML_T> unused, const Opt<T> *ssp) {
    ML_T unused1;
    return c2ml_opt(unused1, ssp) ;
}
