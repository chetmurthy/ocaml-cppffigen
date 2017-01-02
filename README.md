# OCaml C++ FFI Generator

This project is a pre-pre-pre-pre alpha release of a C++
foreign-function-interface generator for Ocaml.  Please use at your own risk.

#### License

This code is released under an Apache 2.0 license </LICENSE>.

## Why do we need another FFI generator?

The first and most important question you ought to be asking yourself as a reader is

  ```
Why do we need another FFI Generator for Ocaml?  Isn't Camlidl enough?  What about Ctypes?
  ```

This is a very fair question, and I can only answer it by telling you
my reasons for writing this tool.  YMMV, and I surely would suggest
that you strongly consider using one of the above-mentioned tools,
before considering this one.

### Reasons for using this tool instead of others.

1. __What about CamlIDL?__ I've been a longtime user of CamlIDL.  It's
   great for C code, when the types are pretty well-behaved and don't
   have complex structures requiring a lot of allocation/deallocation.
   And it doesn't deal at all with C++.
   
2. __What about CTypes?__ This tool's genesis is in my frustration
   with Ctypes.  Ctypes doesn't deal with C++ either.  But originally,
   I was using the [orocksdb][orocksdb] library, and while it worked
   well, I needed to extend it.  `orocksdb` uses Ctypes to interface
   to the Rocksdb C API, and the new operations I wanted to add were
   complex.  Ctypes is complex to understand, and while I'm a
   long-time Caml-Light/Ocaml programmer, I didn't relish the thought
   of figuring out how to allocate/manage/free C memory from Ocaml
   code.  It seemed really complicated.
   
3. __C++'s STL is a great fit for Ocaml__ This is the real reason for
   this project: I've been programming C++ for a number of years, and
   the STL types (`std::string`, `std::vector`, etc) are a great fit
   for Ocaml's type-constructors, and the memory-ownership semantics
   of those types match well with Ocaml.  So it seemed to me that one
   could easily generate the type-directed translation code needed to
   convert complex data-types in C++ to/from ML.

### Reasons to NOT use this tool

1. The code to which you wish to interface is _not_ in C++.  This
   should be obvious.  This tool only works for C++.

2. The C++ code you wish to interface to does not follow the
   [Google C++ Style Guide][Google C++ Style Guide], or uses a lot of
   types that do not follow C++ STL conventions.  This tool leverages
   the memory-ownership rules and semantics of the C++ STL, as
   codified in Google's style guide.


[Google C++ Style Guide]: https://google.github.io/styleguide/cppguide.html
[orocksdb]: https://github.com/domsj/orocksdb

