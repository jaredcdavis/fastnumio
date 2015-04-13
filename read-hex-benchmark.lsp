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



(defun read-sharp-file (filename)
  (with-open-file (stream filename :direction :input)
    (loop do
          (let ((elem (read stream nil nil)))
            (when (not elem)
              (loop-finish))))))

(time (read-sharp-file "/dev/shm/sharp-u32s.txt"))   ;; 1.98 seconds, 0 bytes
(time (read-sharp-file "/dev/shm/sharp-u64s.txt"))   ;; 2.866 seconds, 53 MB
(time (read-sharp-file "/dev/shm/sharp-u128s.txt"))  ;; 6.353 seconds, 1 GB


(time (my-read-file "/dev/shm/u32s.txt"))   ;; .430 seconds, 144 MB allocated (why??)
(time (my-read-file "/dev/shm/u64s.txt"))   ;; .718 seconds, 190 MB allocated
(time (my-read-file "/dev/shm/u128s.txt"))  ;; 1.627 seconds, 271 MB allocated




(defun read-file-lines (filename)
  (let ((len 0))
    (with-open-file (stream filename :direction :input)
      (loop do
            (let ((line (read-line stream nil)))
              (when (not line)
                (loop-finish))
              ;; Otherwise SBCL optimizes things away
              (incf len (length line)))))))

(time (read-file-lines "/dev/shm/sharp-u32s.txt"))   ;; 1.95 seconds, 0 bytes
(time (read-file-lines "/dev/shm/sharp-u64s.txt"))   ;; 2.03 seconds, 80 MB
(time (read-file-lines "/dev/shm/sharp-u128s.txt"))  ;; 2.15 seconds, 144 MB

; So this is a bit depressing, read-line is pretty damn expensive

(defun read-file-chars (filename)
  (with-open-file (stream filename :direction :input)
    (loop do
          (let ((c (read-char stream nil)))
            (when (not c)
              (loop-finish))))))

(time (read-file-chars "/dev/shm/sharp-u32s.txt"))  ;; 0.188 seconds, 0 bytes
(time (read-file-chars "/dev/shm/sharp-u64s.txt"))  ;; 0.314 seconds, 0 bytes
(time (read-file-chars "/dev/shm/sharp-u128s.txt")) ;; 0.574 seconds, 0 bytes

; So that is more promising.

; There is also read-sequence, which may be faster.


(defun read-file-seq (filename)
  (let ((buf (make-array 80 :element-type 'character)))
    (with-open-file (stream filename :direction :input)
      (loop do
            (let ((c (read-sequence buf stream)))
              (when (< c 80)
                (loop-finish)))))))

(time (read-file-seq "/dev/shm/sharp-u32s.txt"))  ;; 0.133 seconds, 0 bytes
(time (read-file-seq "/dev/shm/sharp-u64s.txt"))  ;; 0.221 seconds, 0 bytes
(time (read-file-seq "/dev/shm/sharp-u128s.txt")) ;; 0.396 seconds, 0 bytes

; So that's 1.4x faster, but it's not very easy to write a nice high-level
; interface to it, because we don't know where the EOLs are.


; But we'll either need to unread characters or use peek-char...

(defun read-file-peek-chars (filename)
  (with-open-file (stream filename :direction :input)
    (loop do
          (let ((c (peek-char nil stream nil)))
            (when (not c)
              (loop-finish))
            (setq c (read-char stream nil))))))

(time (read-file-peek-chars "/dev/shm/sharp-u32s.txt"))  ;; 1.06 seconds
(time (read-file-peek-chars "/dev/shm/sharp-u64s.txt"))  ;; 1.78 seconds
(time (read-file-peek-chars "/dev/shm/sharp-u128s.txt")) ;; 3.27 seconds

; So that's horrible.  How can that take so fucking long?

(defun read-file-peek-chars2 (filename)
  (with-open-file (stream filename :direction :input)
    (loop do
          (let ((c (read-char stream nil)))
            (when (not c)
              (loop-finish))
            (when (or (eql c #\Newline)
                      (eql c #\Space))
              (unread-char c stream)
              (read-char stream nil))))))

(time (read-file-peek-chars2 "/dev/shm/sharp-u32s.txt")) ;; 0.228 seconds
(time (read-file-peek-chars2 "/dev/shm/sharp-u64s.txt")) ;; 0.363 seconds
(time (read-file-peek-chars2 "/dev/shm/sharp-u128s.txt")) ;; 0.623 seconds

; That's much better.  Ok, so I guess we need to base our parser on read-char
; and just use unread-char when we encounter a non-hex digit.

; The general idea 


(defun eat-whitespace (stream)
  (let ((char (read-char stream nil)))
    (cond ((not char)
           stream)
          ((or (eql char #\Space)
               (eql char #\Newline))
           stream)
          (t
           (unread-char char stream)
           stream))))

(defun my-read-stream (stream)
  (let ((elem nil))
    (loop do
          (eat-whitespace stream)
          (let ((tmp (read-hex-v1 stream)))
            (if tmp
                (setq elem tmp)
              (loop-finish))))
    elem))

(defun my-read-file (filename)
  (with-open-file (stream filename :direction :input)
    (my-read-stream stream)))


(defun my-read-1 (stream)
  (if 
