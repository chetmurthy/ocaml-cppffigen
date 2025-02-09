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

value_INT c2ml(int v) { return Val_int(v) ; }
value_INT c2ml(unsigned int v) { return Val_int(v) ; }
value_INT c2ml(bool v) { return Val_bool(v) ; }
value_INT c2ml(char v) { return Val_int((int)v) ; }
value_INT c2ml(unsigned char v) { return Val_int((int)v) ; }

//value c2ml(int32_t v) { return caml_copy_int32(v) ; }
//value c2ml(uint32_t v) { return caml_copy_int32(v) ; }
value_INT64 c2ml(int64_t v) { return caml_copy_int64(v) ; }
value_INT64 c2ml(uint64_t v) { return caml_copy_int64(v) ; }

void ml2c(const value_INT v, int *cv) { *cv = Int_val(v.extract()) ; }
void ml2c(const value_INT v, unsigned int *cv) { *cv = Int_val(v.extract()) ; }
void ml2c(const value_INT v, bool *cv) { *cv = Bool_val(v.extract()) ; }
void ml2c(const value_INT v, char *cv) { *cv = (char)Int_val(v.extract()) ; }
void ml2c(const value_INT v, unsigned char *cv) { *cv = (char)Int_val(v.extract()) ; }

//void ml2c(const value v, int32_t *cv) { *cv = Int32_val(v) ; }
//void ml2c(const value v, uint32_t *cv) { *cv = Int32_val(v) ; }
void ml2c(const value_INT64 v, int64_t *cv) { *cv = Int64_val(v.extract()) ; }
void ml2c(const value_INT64 v, uint64_t *cv) { *cv = Int64_val(v.extract()) ; }

value c2ml(const std::string& v) {
  uint32_t size = v.size() ;
  value res = caml_alloc_string(size);
  memmove((void *)String_val(res), (const void *)v.c_str(), size);
  return res ;
}

void ml2c(const value v, std::string *cv) {
  int size = caml_string_length(v) ;
  *cv = std::string(String_val(v), size) ;
}

template<class T, class U>
value c2ml(const T& t, const U& u) {
  CAMLparam0() ;
  CAMLlocal1(mlv) ;
  mlv = caml_alloc_tuple(2) ;
  Store_field(mlv, 0, c2ml(t)) ;
  Store_field(mlv, 1, c2ml(u)) ;
  CAMLreturn(mlv) ;
}
template<class T, class U>
value c2ml(const std::tuple<T, U>& v) {
  return c2ml(std::get<0>(v), std::get<1>(v)) ;
}

template<class T, class U>
void ml2c(const value v, std::tuple<T, U>* cv) {
  ml2c(Field(v, 0), &(std::get<0>(*cv))) ;
  ml2c(Field(v, 1), &(std::get<1>(*cv))) ;
}

template<class T, class U, class V>
value c2ml(const T& t, const U& u, const V& v) {
  CAMLparam0() ;
  CAMLlocal1(mlv) ;
  mlv = caml_alloc_tuple(3) ;
  Store_field(mlv, 0, c2ml(t)) ;
  Store_field(mlv, 1, c2ml(u)) ;
  Store_field(mlv, 2, c2ml(v)) ;
  CAMLreturn(mlv) ;
}
template<class T, class U, class V>
value c2ml(const std::tuple<T, U, V>& v) {
  return c2ml(std::get<0>(v), std::get<1>(v), std::get<2>(v)) ;
}

template<class T, class U, class V>
void ml2c(const value v, std::tuple<T, U, V>* cv) {
  ml2c(Field(v, 0), &(std::get<0>(*cv))) ;
  ml2c(Field(v, 1), &(std::get<1>(*cv))) ;
  ml2c(Field(v, 2), &(std::get<2>(*cv))) ;
}

template<class T>
value c2ml(const std::vector<T>& v) {
  CAMLparam0() ;
  CAMLlocal1(mlv) ;
  mlv = caml_alloc(v.size(), 0) ;
   for(int i = 0 ; i < v.size(); i++) {
    Store_field(mlv, i, c2ml(v[i])) ;
  }
  CAMLreturn(mlv) ;
}

template<typename T>
void ml2c(const value v, std::vector<T>* cv) {
  int n = caml_array_length(v) ;
  cv->clear();
  cv->resize(n) ;
  for(int i = 0 ; i < n ; i++) {
    T tmp ;
    ml2c(Field(v, i), &tmp) ;
    (*cv)[i] = tmp ;
  }
}

template<class T>
value c2ml_opt(const T *&p) {
    CAMLparam0 () ;
  if (NULL == p) {
    CAMLreturn (Val_int(0)) ;
  }
  else {
    CAMLlocal1 (v) ;
    v = caml_alloc_small(1, 0) ;
    Field(v, 0) = c2ml(*p) ;
    CAMLreturn(v) ;
  }
}

template <class T>
value c2ml(const Opt<T> *ssp) {
  return c2ml_opt(ssp) ;
}
