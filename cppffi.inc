/* these definitions probably need to go, but I'll leave them here
until I have more tests/examples */

template<class T>
value c2ml_owned_pointer(const T & p) {
  assert (NULL != p) ;
  value _v1;
  _v1 = caml_alloc_small((sizeof(T) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((T *) Bp_val(_v1)) = p;
  return _v1;
}

template<class T>
void ml2c_owned_pointer(value v, T *cv) {
  *cv = *((T *) Bp_val(v));
}

template<class T>
void ml2c_set_owned_pointer(value v, T cv) {
  *((T *) Bp_val(v)) = cv ;
}

value c2ml(sentinel_INT unused, int v) { return Val_int(v) ; }
value c2ml(sentinel_INT unused, unsigned int v) { return Val_int(v) ; }
value c2ml(sentinel_INT unused, bool v) { return Val_bool(v) ; }
value c2ml(sentinel_INT unused, char v) { return Val_int((int)v) ; }
value c2ml(sentinel_INT unused, unsigned char v) { return Val_int((int)v) ; }

value c2ml(sentinel_INT32 unused, int32_t v) { return caml_copy_int32(v) ; }
value c2ml(sentinel_INT32 unused, uint32_t v) { return caml_copy_int32(v) ; }
value c2ml(sentinel_INT64 unused, int64_t v) { return caml_copy_int64(v) ; }
value c2ml(sentinel_INT64 unused, uint64_t v) { return caml_copy_int64(v) ; }

void ml2c(sentinel_INT unused, value v, int *cv) { *cv = Int_val(v) ; }
void ml2c(sentinel_INT unused, value v, unsigned int *cv) { *cv = Int_val(v) ; }
void ml2c(sentinel_INT unused, value v, bool *cv) { *cv = Bool_val(v) ; }
void ml2c(sentinel_INT unused, value v, char *cv) { *cv = (char)Int_val(v) ; }
void ml2c(sentinel_INT unused, value v, unsigned char *cv) { *cv = (char)Int_val(v) ; }

void ml2c(sentinel_INT32 unused, value v, int32_t *cv) { *cv = Int32_val(v) ; }
void ml2c(sentinel_INT32 unused, value v, uint32_t *cv) { *cv = Int32_val(v) ; }
void ml2c(sentinel_INT64 unused, value v, int64_t *cv) { *cv = Int64_val(v) ; }
void ml2c(sentinel_INT64 unused, value v, uint64_t *cv) { *cv = Int64_val(v) ; }

value c2ml(sentinel_GENERIC unused, const std::string& v) {
  uint32_t size = v.size() ;
  value res = caml_alloc_string(size);
  memmove((void *)String_val(res), (const void *)v.c_str(), size);
  return res ;
}

void ml2c(sentinel_GENERIC unused, value v, std::string *cv) {
  CAMLparam1(v) ;
  int size = caml_string_length(v) ;
  *cv = std::string(String_val(v), size) ;
  CAMLreturn0;
}

template<class T, class U, typename ML_T, typename ML_U>
value c2ml(ML_T unused1, ML_U unused2, const T& t, const U& u) {
  CAMLparam0() ;
  CAMLlocal1(mlv) ;
  mlv = caml_alloc_tuple(2) ;
  Store_field(mlv, 0, c2ml(ML_T(), t)) ;
  Store_field(mlv, 1, c2ml(ML_U(), u)) ;
  CAMLreturn(mlv) ;
}
template<typename T, typename U, typename ML_T, typename ML_U>
value c2ml(sentinel_TUPLE2<ML_T, ML_U> unused, const std::tuple<T, U>& v) {
  return c2ml(ML_T(), ML_U(), std::get<0>(v), std::get<1>(v)) ;
}

template<class T, class U, typename ML_T, typename ML_U>
void ml2c(sentinel_TUPLE2<ML_T, ML_U> unused, value v, std::tuple<T, U>* cv) {
  CAMLparam1(v) ;
  ml2c(ML_T(), Field(v, 0), &(std::get<0>(*cv))) ;
  ml2c(ML_U(), Field(v, 1), &(std::get<1>(*cv))) ;
  CAMLreturn0;
}

template<class T, class U, class V,
	 typename ML_T,
	 typename ML_U,
	 typename ML_V
        >
value c2ml(ML_T unusedt, ML_U unusedu, ML_V unusedv, const T& t, const U& u, const V& v) {
  CAMLparam0() ;
  CAMLlocal1(mlv) ;
  mlv = caml_alloc_tuple(3) ;
  Store_field(mlv, 0, c2ml(ML_T(), t)) ;
  Store_field(mlv, 1, c2ml(ML_U(), u)) ;
  Store_field(mlv, 2, c2ml(ML_V(), v)) ;
  CAMLreturn(mlv) ;
}
template<class T, class U, class V,
	 typename ML_T,
	 typename ML_U,
	 typename ML_V
        >
value c2ml(sentinel_TUPLE3<ML_T, ML_U, ML_V> unused, const std::tuple<T, U, V>& v) {
  return c2ml(ML_T(), ML_U(), ML_V(), std::get<0>(v), std::get<1>(v), std::get<2>(v)) ;
}

template<class T, class U, class V,
	 typename ML_T,
	 typename ML_U,
	 typename ML_V
        >
void ml2c(sentinel_TUPLE3<ML_T, ML_U, ML_V> unused, value v, std::tuple<T, U, V>* cv) {
  CAMLparam1(v);
  ml2c(ML_T(), Field(v, 0), &(std::get<0>(*cv))) ;
  ml2c(ML_U(), Field(v, 1), &(std::get<1>(*cv))) ;
  ml2c(ML_V(), Field(v, 2), &(std::get<2>(*cv))) ;
  CAMLreturn0;
}

template<class T, typename ML_T>
value c2ml(sentinel_ARRAY<ML_T> unused, const std::vector<T>& v) {
  CAMLparam0() ;
  CAMLlocal1(mlv) ;
  mlv = caml_alloc(v.size(), 0) ;
  for(int i = 0 ; i < v.size(); i++) {
    Store_field(mlv, i, c2ml(ML_T(), v[i])) ;
  }
  CAMLreturn(mlv) ;
}

template<typename T, typename ML_T>
void ml2c(sentinel_ARRAY<ML_T> unused, value v, std::vector<T>* cv) {
  CAMLparam1(v) ;
  int n = caml_array_length(v) ;
  cv->clear();
  cv->resize(n) ;
  for(int i = 0 ; i < n ; i++) {
    T tmp ;
    ml2c(ML_T(), Field(v, i), &tmp) ;
    (*cv)[i] = tmp ;
  }
  CAMLreturn0;
}

template <class T, typename ML_T>
value c2ml(sentinel_OPTION<ML_T> unused, const std::optional<T>& ssp) {
    CAMLparam0() ;
    CAMLlocal1 (v) ;
    if (! ssp.has_value()) {
       CAMLreturn(Val_int(0)) ;       
    }
    v = caml_alloc_small(1, 0) ;
    Field(v, 0) = c2ml(ML_T(), ssp.value()) ;
    CAMLreturn(v) ;
}

template<typename ML_T, typename T>
void ml2c(sentinel_OPTION<ML_T>  unused, value v, std::optional<T>* cv) {
  CAMLparam1(v) ;
  if (Is_long(v) && Long_val(v) == 0) {
    cv->reset() ;
  }
  else {
  T sub_cv;
    ml2c(ML_T(), Field(v,0), &sub_cv) ;
    *cv = std::optional(sub_cv) ;
  }
  CAMLreturn0;
}