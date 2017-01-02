
/* by construction, [it] should NEVER be NULL */
template<class T>
struct Opt ;

template<class T>
struct OptWrap ;

template<class T>
value c2ml_owned_pointer(const T & p);

template<class T>
void ml2c_owned_pointer(const value v, T *cv);

template<class T>
void ml2c_set_owned_pointer(value v, T cv);

template<class T>
value c2ml_opt_owned_pointer(const T *& p);

value c2ml(int v);
value c2ml(bool v);
value c2ml(char v);

void ml2c(const value v, int *cv);
void ml2c(const value v, bool *cv);
void ml2c(const value v, char *cv);

value c2ml(const std::string& v);

void ml2c(const value v, std::string *cv);

template<class T, class U>
  value c2ml(const T& t, const U& u);

template<class T, class U>
  value c2ml(const std::tuple<T, U>& v) ;

template<class T, class U>
  void ml2c(const value v, std::tuple<T, U>* cv) ;

template<class T, class U, class V>
  value c2ml(const T& t, const U& u, const V& v) ;

template<class T, class U, class V>
  value c2ml(const std::tuple<T, U, V>& v) ;

template<class T, class U, class V>
  void ml2c(const value v, std::tuple<T, U, V>* cv) ;

template<class T>
value c2ml(const std::vector<T>& v) ;

template<class T>
void ml2c(const value v, std::vector<T>* cv) ;

template<class T>
value c2ml_opt(const T *&p) ;

template <class T>
value c2ml(const Opt<T> *ssp) ;
