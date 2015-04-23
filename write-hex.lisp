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

(in-package "FASTNUMIO")

(declaim (optimize (speed 3) (space 1) (safety 0)))

; ----------------------------------------------------------------------------
;
;                          Supporting Utilities
;
; ----------------------------------------------------------------------------

(defmacro fast-u32-p (x)
  "Maybe faster version of (< x (expt 2 32)).
   X must be an (integer 0 *).

   Performance test on CCL Linux X86-64:

                     n=32   n=64   n=128  n=512
     fast-u32-p      .133s  .331s  .331s  .349s
     (< elem 2^32)   .129s  .842s  .821s  .822s

   Performance test code:

   (let* ((n     512)
          (limit (expt 2 n))
          (data  (loop for i from 1 to 10000 collect (random limit))))
     (time (loop for i fixnum from 1 to 10000 do
                 (loop for elem in data do
                       (fast-u32-p elem))))
     (time (loop for i fixnum from 1 to 10000 do
                 (loop for elem in data do
                       (< elem #.(expt 2 32))))))

   This is fast on CCL because fixnum checking is just tag checking, while
   arbitrary-precision < comparison is (comparatively) slow.

   It looks like this is not a win on SBCL, so we only use the fancy
   definition on CCL."

  #+ccl
  (cond ((typep (expt 2 32) 'fixnum)
         `(and (typep ,x 'fixnum)
               (< (the fixnum ,x) ,(expt 2 32))))
        (t
         ;; No way to fixnum optimize it.
         `(< ,x ,(expt 2 32))))

  #-ccl
  `(< ,x ,(expt 2 32)))

(assert (fast-u32-p 0))
(assert (fast-u32-p 1))
(assert (fast-u32-p (+ -2 (expt 2 32))))
(assert (fast-u32-p (+ -1 (expt 2 32))))
(assert (not (fast-u32-p (+ 0 (expt 2 32)))))
(assert (not (fast-u32-p (+ 1 (expt 2 32)))))
(assert (not (fast-u32-p (+ 2 (expt 2 32)))))


(defmacro fast-u60-p (x)
  "Maybe faster version of (< x (expt 2 60)).
   X must be an (integer 0 *).

   Performance test on CCL Linux X86-64:

                      n=32   n=64   n=128  n=512
   fast-u60-p    --   0.12s  0.31s  0.31s  0.34s
   (< elem 2^30) --   0.71s  2.07s  1.58s  1.59s

   Performance test code:

   (let* ((n     512)
          (limit (expt 2 n))
          (data  (loop for i from 1 to 10000 collect (random limit))))
     (time (loop for i fixnum from 1 to 10000 do
                 (loop for elem in data do
                       (fast-u60-p elem))))
     (time (loop for i fixnum from 1 to 10000 do
                 (loop for elem in data do
                       (< elem #.(expt 2 60))))))

   The goal is to reduce arbitrary-precision (< x (expt 2 60)) checking to just
   a tag comparison.

   It looks like this is not a win on SBCL, so we only use the fancy definition
   on CCL."
  #+ccl
  (cond ((and (typep (expt 2 59) 'fixnum)
              (not (typep (expt 2 60) 'fixnum)))
         ;; This Lisp has its fixnum boundary exactly at 2^60, so we can just
         ;; check whether X is a fixnum.
         `(typep ,x 'fixnum))
        ((typep (expt 2 60) 'fixnum)
         ;; This Lisp has fixnums that beyond 2^60.  We can check whether
         ;; X is a fixnum in range.
         `(and (typep ,x 'fixnum)
               (< (the fixnum ,x) ,(expt 2 60))))
        (t
         ;; No way to fixnum optimize it.
         `(< ,x ,(expt 2 60))))

  #-ccl
  `(< ,x ,(expt 2 60)))

(assert (fast-u60-p 0))
(assert (fast-u60-p 1))
(assert (fast-u60-p (+ -2 (expt 2 60))))
(assert (fast-u60-p (+ -1 (expt 2 60))))
(assert (not (fast-u60-p (+ 0 (expt 2 60)))))
(assert (not (fast-u60-p (+ 1 (expt 2 60)))))
(assert (not (fast-u60-p (+ 2 (expt 2 60)))))


(declaim (inline hex-digit-to-char))
(defun hex-digit-to-char (n)
  (declare (type (integer 0 15) n))
  "Convert an integer in [0, 15] to a hex character.
   Adapted from acl2:books/std/strings/hex.lisp"
  (if (< n 10)
      (code-char (the (unsigned-byte 8) (+ 48 n)))
    ;; Naively this is (code-char A) + N-10
    ;; But we merge (code-char A) == 65 and -10 together to get 55.
    (code-char (the (unsigned-byte 8) (+ 55 n)))))

(assert (equal (hex-digit-to-char 0) #\0))
(assert (equal (hex-digit-to-char 1) #\1))
(assert (equal (hex-digit-to-char 2) #\2))
(assert (equal (hex-digit-to-char 3) #\3))
(assert (equal (hex-digit-to-char 4) #\4))
(assert (equal (hex-digit-to-char 5) #\5))
(assert (equal (hex-digit-to-char 6) #\6))
(assert (equal (hex-digit-to-char 7) #\7))
(assert (equal (hex-digit-to-char 8) #\8))
(assert (equal (hex-digit-to-char 9) #\9))
(assert (equal (hex-digit-to-char #xA) #\A))
(assert (equal (hex-digit-to-char #xB) #\B))
(assert (equal (hex-digit-to-char #xC) #\C))
(assert (equal (hex-digit-to-char #xD) #\D))
(assert (equal (hex-digit-to-char #xE) #\E))
(assert (equal (hex-digit-to-char #xF) #\F))


; ----------------------------------------------------------------------------
;
;                           Fast Hex Printing
;
; ----------------------------------------------------------------------------

; Rudimentary testing suggests that printing out whole strings with
; write-string is much faster than printing individual characters with
; write-char on CCL.
;
; The basic idea of write-hex is to split up the value to be printed into
; 60-bit (fixnum) blocks, build strings that contain the corresponding
; characters, and then print these strings out all at once.
;
; This works kind of well.  It is very fast for small numbers and was also
; better than the other methods for bignums.  But even despite dynamic-extent
; declarations, it still uses an awful lot of memory.

(defun write-hex-u60-without-leading-zeroes (val stream)
  (declare (type (unsigned-byte 60) val))
  ;; This version is for values under 2^60, and omits leading zeroes where
  ;; possible.
  (if (eql val 0)
      (write-char #\0 stream)
    (let ((pos    1) ;; **see below
          (shift -56)
          (nibble 0)
          (arr (make-array 15 :element-type 'character)))
      (declare (type string arr)
               (dynamic-extent arr)
               (type fixnum pos)
               (type fixnum shift)
               (type (unsigned-byte 4) nibble))
      ;; Skip past any leading zeroes.  Note that we already checked for the
      ;; all-zero case above, so we know a nonzero digit exists and that we
      ;; will eventually exit the loop.
      (loop do
            (setq nibble
                  (the (unsigned-byte 4)
                       (logand #xF (the (unsigned-byte 60)
                                        (ash (the (unsigned-byte 60) val)
                                             (the (integer -56 0) shift))))))
            (incf shift 4)
            (unless (eql nibble 0)
              (loop-finish)))
      ;; At this point we know we are standing at a nonzero digit and that
      ;; its value is already in nibble.  Install its value into the array.
      (setf (schar arr 0) (hex-digit-to-char nibble))
      ;; ** above we initialized pos to 1, so we don't need to increment
      ;; it here.  Shift has also already been incremented.
      (loop do
            (when (> shift 0)
              (loop-finish))
            (setq nibble
                  (the (unsigned-byte 4)
                       (logand #xF (the (unsigned-byte 60)
                                        (ash (the (unsigned-byte 60) val)
                                             (the (integer -56 0) shift))))))
            (setf (schar arr pos) (hex-digit-to-char nibble))
            (incf pos)
            (incf shift 4))
      ;; At the end of all of this, the array is populated with the digits
      ;; we want to print and POS says how many we need.  So write them.
      (write-string arr stream :end pos)))
  stream)

(defun write-hex-u60-with-leading-zeroes (val stream)
  (declare (type (unsigned-byte 60) val))
  ;; This version prints out a fixnum-sized chunk but doesn't try to avoid
  ;; printing leading zeroes.  This is useful for printing subsequent blocks of
  ;; a bignum.
  (let ((pos    0)
        (shift -56)
        (nibble 0)
        (arr (make-array 15 :element-type 'character)))
    (declare (type string arr)
             (dynamic-extent arr)
             (type fixnum pos)
             (type fixnum shift)
             (type (unsigned-byte 4) nibble))
    (loop do
          (when (> shift 0)
            (loop-finish))
          (setq nibble
                (the (unsigned-byte 4)
                     (logand #xF (the (unsigned-byte 60)
                                      (ash (the (unsigned-byte 60) val)
                                           (the (integer -56 0) shift))))))
          (incf shift 4)
          (setf (schar arr pos) (hex-digit-to-char nibble))
          (incf pos))
    ;; At the end of all of this, the array is populated with the digits
    ;; we want to print and POS says how many we need.  So write them.
    (write-string arr stream)))

(defun write-hex-main (val stream)
  (declare (type unsigned-byte val))
  (if (fast-u60-p val)
      (write-hex-u60-without-leading-zeroes val stream)
    (let ((high (the unsigned-byte (ash val -60)))
          (low  (the (unsigned-byte 60) (logand val #.(1- (expt 2 60))))))
      (declare (type unsigned-byte high)
               (type (unsigned-byte 60) low)
               ;; Disappointingly we still get memory allocation here.
               (dynamic-extent high)
               (dynamic-extent low))
      (write-hex-main high stream)
      (write-hex-u60-with-leading-zeroes low stream)))
  stream)

(declaim (inline write-hex))
(defun write-hex (val stream)
  (declare (type unsigned-byte val))
  (if (fast-u60-p val)
      (write-hex-u60-without-leading-zeroes val stream)
    (write-hex-main val stream)))



; ----------------------------------------------------------------------------
;
;                      Scary Unsafe Hex Printing
;
; ----------------------------------------------------------------------------

; It seems that to do better, we have to go under the hood.  Below is some
; horrible code that exploits the underlying bignum representations of CCL and
; SBCL on 64-bit Linux.  This is much faster and creates no garbage because we
; can avoid creating new bignums.  However, it is scary because it relies on
; internal functionality that might change.  Maybe we can eventually get
; routines like these built into the Lisps.

(defun write-hex-u32-without-leading-zeroes (val stream)
  ;; Completely portable.
  (declare (type (unsigned-byte 32) val))
  (if (eql val 0)
      (write-char #\0 stream)
    (let ((pos    1) ;; **see below
          (shift -28)
          (nibble 0)
          (arr (make-array 8 :element-type 'character)))
      (declare (type string arr)
               (dynamic-extent arr)
               (type (unsigned-byte 32) pos)
               (type (signed-byte 32)   shift)
               (type (unsigned-byte 4)  nibble))
      ;; Skip past any leading zeroes.  Note that we already checked for the
      ;; all-zero case above, so we know a nonzero digit exists and that we
      ;; will eventually exit the loop.
      (loop do
            (setq nibble
                  (the (unsigned-byte 4)
                       (logand #xF (the (unsigned-byte 32)
                                        (ash (the (unsigned-byte 32) val)
                                             (the (integer -28 0) shift))))))
            (incf shift 4)
            (unless (eql nibble 0)
              (loop-finish)))
      ;; At this point we know we are standing at a nonzero digit and that
      ;; its value is already in nibble.  Install its value into the array.
      (setf (schar arr 0) (hex-digit-to-char nibble))
      ;; ** above we initialized pos to 1, so we don't need to increment
      ;; it here.  Shift has also already been incremented.
      (loop do
            (when (> shift 0)
              (loop-finish))
            (setq nibble
                  (the (unsigned-byte 4)
                       (logand #xF (the (unsigned-byte 32)
                                        (ash (the (unsigned-byte 32) val)
                                             (the (integer -28 0) shift))))))
            (setf (schar arr pos) (hex-digit-to-char nibble))
            (incf pos)
            (incf shift 4))
      ;; At the end of all of this, the array is populated with the digits
      ;; we want to print and POS says how many we need.  So write them.
      (write-string arr stream :end pos)))
  stream)

(defun write-hex-u32-with-leading-zeroes (val stream)
  ;; Completely portable.
  (declare (type (unsigned-byte 32) val))
  (let ((pos    0)
        (shift -28)
        (nibble 0)
        (arr (make-array 8 :element-type 'character)))
    (declare (type string arr)
             (dynamic-extent arr)
             (type fixnum pos)
             (type fixnum shift)
             (type (unsigned-byte 4) nibble))
    (loop do
          (when (> shift 0)
            (loop-finish))
          (setq nibble
                (the (unsigned-byte 4)
                     (logand #xF (the (unsigned-byte 32)
                                      (ash (the (unsigned-byte 32) val)
                                           (the (integer -28 0) shift))))))
          (incf shift 4)
          (setf (schar arr pos) (hex-digit-to-char nibble))
          (incf pos))
    ;; At the end of all of this, the array is populated with the digits
    ;; we want to print and POS says how many we need.  So write them.
    (write-string arr stream)))



; CCL specific bignum printing.
;
; Note: CCL on Linux X86-64 represents bignums as vectors of 32-bit numbers,
; with the least significant chunks coming first.

#+(and Clozure x86-64)
(progn
  ;; Make sure we still properly understand the fixnum/bignum boundary.
  ;; If this changes, scary-unsafe-write-hex is wrong.
  (assert (typep (1- (expt 2 60)) 'fixnum))
  (assert (not (typep (expt 2 60) 'fixnum))))

#+(and Clozure x86-64)
(defun scary-unsafe-write-hex-bignum (val stream)
  ;; Assumption: val must be a bignum.
  ;; Assumption: val must be nonzero.
  (let ((pos (ccl::uvsize val))
        (chunk))
    (declare (type fixnum pos)
             (type (unsigned-byte 32) chunk))
    ;; It seems unlikely that CCL would create bignums with zero chunks at the
    ;; high end, but just in case, we'll go ahead and skip past any leading
    ;; pure-zero chunks.
    (loop do
          (decf pos)
          (setq chunk (ccl::uvref val pos))
          (unless (eql chunk 0)
            (loop-finish)))
    ;; POS now points to the first nonzero chunk.
    ;; CHUNK is the contents of the first nonzero chunk.
    (write-hex-u32-without-leading-zeroes chunk stream)
    ;; We now need to print the remaining chunks, if any, in full.
    (loop do
          (decf pos)
          (when (< pos 0)
            (loop-finish))
          (setq chunk (ccl::uvref val pos))
          (write-hex-u32-with-leading-zeroes chunk stream))))


; SBCL specific bignum printing.
;
; Note: SBCL on Linux X86-64 represents bignums as vectors of 64-bit 'digits',
; with the least significant digit in place 0.

#+(and sbcl x86-64)
(progn
  ;; Basic sanity checking to see if we still understand the internal API.
  (assert (equal sb-bignum::digit-size 64))
  (assert (equal (sb-bignum::%bignum-ref (1- (expt 2 80)) 0) (1- (expt 2 64))))
  (assert (equal (sb-bignum::%bignum-ref (1- (expt 2 80)) 1) (1- (expt 2 16))))
  (assert (typep (1- (expt 2 64)) 'sb-bignum::bignum-element-type))
  (let* ((x      #xfeedf00ddeadd00ddeadbeef99998888)
         (digit  (sb-bignum::%bignum-ref x 0))
         (high32 (sb-bignum::%digit-logical-shift-right digit 32))
         (low32  (sb-bignum::%logand digit #xFFFFFFFF)))
    (assert (typep high32 'fixnum))
    (assert (typep low32 'fixnum))
    (assert (typep high32 '(unsigned-byte 32)))
    (assert (typep low32 '(unsigned-byte 32)))
    (assert (equal high32 #xdeadbeef))
    (assert (equal low32 #x99998888))))

#+(and sbcl x86-64)
(defun write-hex-bignum-digit-with-leading-zeroes (digit stream)
  (declare (type sb-bignum::bignum-element-type digit))
  (let ((high32 (sb-bignum::%digit-logical-shift-right digit 32))
        (low32  (sb-bignum::%logand digit #xFFFFFFFF)))
    (declare (type (unsigned-byte 32) high32 low32))
    (write-hex-u32-with-leading-zeroes high32 stream)
    (write-hex-u32-with-leading-zeroes low32 stream)
    stream))

#+(and sbcl x86-64)
(defun write-hex-bignum-digit-without-leading-zeroes (digit stream)
  (declare (type sb-bignum::bignum-element-type digit))
  ;; If digit is nonzero, we print it and return T.
  ;; If digit is zero,    we do not print anything and return NIL.
  (let* ((high32 (sb-bignum::%digit-logical-shift-right digit 32))
         (low32  (sb-bignum::%logand digit #xFFFFFFFF)))
    (declare (type (unsigned-byte 32) high32 low32))
    (if (eql high32 0)
        (if (eql low32 0)
            nil
          (progn (write-hex-u32-without-leading-zeroes low32 stream)
                 t))
      (progn
        (write-hex-u32-without-leading-zeroes high32 stream)
        (write-hex-u32-with-leading-zeroes    low32 stream)
        t))))

#+(and sbcl x86-64)
(defun scary-unsafe-write-hex-bignum (val stream)
  ;; Assumption: val must be a bignum.
  ;; Assumption: val must be nonzero.
  (let ((pos (sb-bignum::%bignum-length val))
        (digit))
    (declare (type fixnum pos))
    ;; Print chunks skipping leading zeroes until we've printed at least
    ;; something.  It seems unlikely that SBCL would create bignums with
    ;; leading zero chunks, but who knows.
    (loop do
          (decf pos)
          (setq digit (sb-bignum::%bignum-ref val pos))
          (when (write-hex-bignum-digit-without-leading-zeroes digit stream)
            ;; Printed something, so subsequent chunks must be printed with
            ;; zeroes enabled.
            (loop-finish)))
    ;; Print any remaining chunks in full.
    (loop do
          (decf pos)
          (when (< pos 0)
            (loop-finish))
          (setq digit (sb-bignum::%bignum-ref val pos))
          (write-hex-bignum-digit-with-leading-zeroes digit stream))))


; Wrap up:

(declaim (inline scary-unsafe-write-hex))
(defun scary-unsafe-write-hex (val stream)
  (declare (type unsigned-byte val))

  #+(and (not (and Clozure x86-64))
         (not (and sbcl x86-64)))
  (write-hex val stream)

  #+(and Clozure x86-64)
  ;; Any fixnums can be handled with the ordinary 60-bit printer.
  (if (fast-u60-p val)
      (write-hex-u60-without-leading-zeroes val stream)
    ;; Else we know it's a bignum because we checked, above, that
    ;; fixnums are still 60 bits.
    (scary-unsafe-write-hex-bignum val stream))

  #+(and sbcl x86-64)
  (if (typep val 'fixnum)
      ;; SBCL on x86-64 has some fixnums that exceed 2^60, so we don't know
      ;; it's a u60.  Just fall back to the generic write-hex code, then.
      ;; BOZO this may be especially lousy for [2^60, most-positive-fixnum]
      (write-hex val stream)
    (scary-unsafe-write-hex-bignum val stream))

  stream)


; ----------------------------------------------------------------------------
;
;                        Basic Correctness Tests
;
; ----------------------------------------------------------------------------

(let ((tests (append
              (list #xbeef
                    #x1beef
                    #x12beef
                    #x123beef
                    #xdeadbeef
                    #x1deadbeef
                    #x12deadbeef
                    #x123deadbeef
                    #x1234deadbeef
                    #x12345deadbeef
                    #x123456deadbeef
                    #x1234567deadbeef
                    (- (expt 2 60) 3)
                    (- (expt 2 60) 2)
                    (- (expt 2 60) 1)
                    (expt 2 60)
                    (+ (expt 2 60) 1)
                    (+ (expt 2 60) 2)
                    (+ (expt 2 60) 3)
                    (+ (expt 2 60) 4)
                    (+ (expt 2 60) 15)
                    (+ (expt 2 60) 16)
                    (+ (expt 2 60) #xf0)
                    (expt 2 64)
                    (1- (expt 2 64))
                    (expt 2 80)
                    (1- (expt 2 80)))
              (loop for i from 0 to 100 collect i)
              (loop for i from 1 to 100 collect (random (expt 2 64)))
              (loop for i from 1 to 100 collect (random (expt 2 1024))))))
  (loop for test in tests do
        (let ((spec (let ((stream (make-string-output-stream)))
                      (format stream "~x" test)
                      (get-output-stream-string stream)))
              (v1 (let ((stream (make-string-output-stream)))
                    (write-hex test stream)
                    (get-output-stream-string stream)))
              (v2 (let ((stream (make-string-output-stream)))
                    (scary-unsafe-write-hex test stream)
                    (get-output-stream-string stream))))
          (or (equal spec v1)
              (error "V1 Failure: ~x --> spec ~s !==  impl ~s" test spec v1))
          (or (equal spec v2)
              (error "V2 Failure: ~x --> spec ~s !==  impl ~s" test spec v2))))
  :ok)


