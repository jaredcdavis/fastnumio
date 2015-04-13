; Fastnumio - Efficient hex string I/O ops for Common Lisp streams
; Copyright (C) 2015 Centaur Technology
;
; Contact:
;   Centaur Technology Formal Verification Group
;   7600-C N. Capital of Texas Highway, Suite 300, Austin, TX 78731, USA.
;   http://www.centtech.com/
;
; License: (An MIT/X11-style license)
;
;   Permission is hereby granted, free of charge, to any person obtaining a
;   copy of this software and associated documentation files (the "Software"),
;   to deal in the Software without restriction, including without limitation
;   the rights to use, copy, modify, merge, publish, distribute, sublicense,
;   and/or sell copies of the Software, and to permit persons to whom the
;   Software is furnished to do so, subject to the following conditions:
;
;   The above copyright notice and this permission notice shall be included in
;   all copies or substantial portions of the Software.
;
;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;   DEALINGS IN THE SOFTWARE.
;
; Original author: Jared Davis <jared@centtech.com>

(ql:quickload :fastnumio)
(in-package "FASTNUMIO")

;; read-hex performance testing

; We need some test data.

(with-open-file (stream "u32s.txt"
                        :direction :output
                        :if-exists :supersede)
  (loop for i fixnum from 1 to 1000000 do
        (format stream "~x~%" (random (expt 2 32)))))

(with-open-file (stream "sharp-u32s.txt"
                        :direction :output
                        :if-exists :supersede)
  (loop for i fixnum from 1 to 1000000 do
        (format stream "#x~x~%" (random (expt 2 32)))))

(with-open-file (stream "u64s.txt"
                        :direction :output
                        :if-exists :supersede)
  (loop for i fixnum from 1 to 1000000 do
        (format stream "~x~%" (random (expt 2 64)))))

(with-open-file (stream "sharp-u64s.txt"
                        :direction :output
                        :if-exists :supersede)
  (loop for i fixnum from 1 to 1000000 do
        (format stream "#x~x~%" (random (expt 2 64)))))

(with-open-file (stream "u128s.txt"
                        :direction :output
                        :if-exists :supersede)
  (loop for i fixnum from 1 to 1000000 do
        (format stream "~x~%" (random (expt 2 128)))))

(with-open-file (stream "sharp-u128s.txt"
                        :direction :output
                        :if-exists :supersede)
  (loop for i fixnum from 1 to 1000000 do
        (format stream "#x~x~%" (random (expt 2 128)))))