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

;; Functions for reading frames from a collection of bytes

(in-package :ax25)

(defun unstuff-bits (bytes &optional current-byte current-bit new-byte new-bit one-count result)
  "Removes bit stuffing from the byte sequence and returns the new byte sequence"
  (cond
    ((and (not current-bit) (not bytes))
     ;; No input, so return an empty list
     '())
    ((not current-bit)
     ;; Start recursion
     (unstuff-bits (rest bytes) (first bytes) 0 0 0 0 '()))
    ((not current-byte)
     ;; Finished
     (reverse result))
    ((= current-bit 8)
     ;; Finished with the current byte
     (unstuff-bits (rest bytes) (first bytes) 0 new-byte new-bit one-count result))
    ((= new-bit 8)
     ;; Finished with the new byte
     (push new-byte result)
     (unstuff-bits bytes current-byte current-bit 0 0 one-count result))
    ((= one-count 5)
     ;; Skip the next bit and reset one count
     (unstuff-bits bytes current-byte (+ 1 current-bit) new-byte new-bit 0 result))
    ((= (ldb (byte 1 current-bit) current-byte) 0)
     ;; Next bit is a zero, add it and reset the one count
     (setf (ldb (byte 1 new-bit) new-byte) 0)
     (unstuff-bits bytes current-byte (+ 1 current-bit) new-byte (+ 1 new-bit) 0 result))
    ('t
     ;; Next bit is a one
     (setf (ldb (byte 1 new-bit) new-byte) 1)
     (unstuff-bits bytes current-byte (+ 1 current-bit) new-byte (+ 1 new-bit) (+ 1 one-count) result))))

;; TODO: Implement deframer
(defun read-frame ()
  "Reads a frame header from the given byte sequence"
  (make-frame)
)

