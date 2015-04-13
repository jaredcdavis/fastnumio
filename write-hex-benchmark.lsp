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
(ql:quickload :trivial-garbage)
(in-package "FASTNUMIO")

;; write-hex performance testing

(defun test-builtin (ntimes nums stream)
  (declare (type fixnum ntimes))
  (loop for i fixnum from 1 to ntimes do
        (loop for num in nums do
              (format stream "~x" num))))

(defun test-v1 (ntimes nums stream)
  (declare (type fixnum ntimes))
  (loop for i fixnum from 1 to ntimes do
        (loop for num in nums do
              (write-hex-v1 num stream))))

(defun test-v2 (ntimes nums stream)
  (declare (type fixnum ntimes))
  (loop for i fixnum from 1 to ntimes do
        (loop for num in nums do
              (write-hex num stream))))

(defun gc ()
  (tg::gc :full t :verbose nil))

#+Clozure
(ccl::gc-verbose nil)

(defmacro my-time (form)
  `(let ((start (get-internal-real-time))
         (blah  (time ,form))
         (end   (get-internal-real-time)))
     (declare (ignore blah))
     (/ (coerce (- end start) 'float) internal-time-units-per-second)))

(defparameter *times*
  (loop for x in '((32  . 1000000) ;; 2^N . NTIMES
                   (60  . 1000000)
                   (64  . 800000)
                   (80  . 600000)
                   (128 . 500000)
                   (256 . 300000)
                   (512 . 150000))
        collect
        (let* ((n      (car x))
               (ntimes (cdr x))
               (limit  (expt 2 n))
               (nums   (loop for i from 1 to 10 collect (random limit))))
          (format t "~%  --- Testing hex writing of numbers up to 2^~d --- ~%" n)
          (with-open-file (stream "/dev/null"
                                  :direction :output
                                  :if-exists :append)
            (let* ((v2-time  (progn (gc) (my-time (test-v2 ntimes nums stream))))
                   (v1-time  (progn (gc) (my-time (test-v1 ntimes nums stream))))
                   (fmt-time (progn (gc) (my-time (test-builtin ntimes nums stream)))))
              (list n fmt-time v1-time v2-time))))))

(progn
  (format t "~%")
  (format t "         N         FMT         V1         V2      Speedup~%")
  (format t "-----------------------------------------------------------~%")
  (loop for elem in *times* do
        (let* ((n   (first elem))
               (fmt (second elem))
               (v1  (third elem))
               (v2  (fourth elem)))
          (format t "~10D  ~10,2F ~10,2F ~10,2F   ~10,2F~%"
                  n fmt v1 v2
                  (if (< fmt v2)
                      (- (/ v2 fmt))
                    (/ fmt v2)))))
  (format t "-----------------------------------------------------------~%")
  (format t "~%"))


