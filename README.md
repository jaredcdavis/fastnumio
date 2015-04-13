# fastnumio

## Work in progress!! not yet ready for public consumption

This library provides fast Common Lisp routines for

 - printing natural numbers (fixnums or bignums) to output streams
   in hexadecimal format (e.g., `DEADBEEF`)

 - reading hexadecimal numbers in from input streams (to create
   fixnums or bignums)

These routines are optimized for 64-bit CCL on X86-64.  They may not be
especially optimal on other Lisps.  Patches are (of course) welcome.


## API

### `(write-hex val output-stream) --> stream`

  - `val` must be a non-negative integer.
  - `output-stream` must be an output stream.

This is like `(format stream "~x" val)`.  It prints the hexadecimal encoding of
`val` to `output-stream`.  Digits `A`-`F` are printed in upper-case.

On 64-bit CCL, `write-hex` is perhaps 4-7x faster than `(format stream "~x"
val)` and, unlike `format`, allocate no memory.

On 64-bit SBCL, `write-hex` is perhaps 2-4x faster than `format`.  It may use
more memory than `format` on large bignums (e.g., beyond 2^128).




### Authorship, License

Copyright (C) 2015 [Centaur Technology](http://www.centtech.com)

Original author: [Jared Davis](mailto:jared.c.davis@gmail.com)

MIT/X11-style [LICENSE](LICENSE).



