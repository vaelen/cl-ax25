;; MIT License
;;
;; Copyright (c) 2019 Andrew C. Young
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; CRC-CCITT as implemented by AX.25

(in-package :ax25)

(defconstant initial-crc '#xffff)
(defconstant poly '#x8408)

(defun lognot-16 (x)
  "Returns the 16 bit bitwise complement."
  (declare (type integer x))
  (loop for i in '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
        do (setf (ldb (byte 1 i) x)
                 (if (logbitp 0 (ldb (byte 1 i) x)) 0 1)))
  x)

(defparameter *crc-table*
  #(#x0000 #x1081 #x2102 #x3183
    #x4204 #x5285 #x6306 #x7387
    #x8408 #x9489 #xa50a #xb58b
    #xc60c #xd68d #xe70e #xf78f))

(defun crc16 (data)
  "Generate the CRC16/X25 value for the given byte sequence."
  (let ((crc #xffff))
    (loop for b across (coerce data 'vector)
          do (progn
               (setf crc (logxor (ash crc -4)
                                 (aref *crc-table*
                                       (logxor
                                        (logand crc #xf)
                                        (logand b #xf)))))
               (setf crc (logxor (ash crc -4)
                                 (aref *crc-table*
                                       (logxor
                                        (logand crc #xf)
                                        (ash b -4)))))))
    (lognot-16 crc)))

(defun fcs (data)
  "Returns an AX.25 Frame Check Sequence for the given byte sequence."
  (let ((crc (crc16 data)))
    (list (ldb (byte 8 0) crc) (ldb (byte 8 8) crc))))
