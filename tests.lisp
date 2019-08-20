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

;; Test Suite

(in-package :ax25)

(defun do-test (description expected value &optional (pred #'equal))
  "Compare the result to the expected value using predicate pred."
  (if (apply pred (list expected value))
      (format 't "~&. SUCCESS: ~A" description)
      (format 't "~&! FAILURE: ~A~%~10TExpected: ~X~%~10TResult:   ~X" description expected value)))

;; See https://crccalc.com/ "CRC-16/X-25" for correct values.
;; See also https://gist.github.com/andresv/4611897

(defun test-suite ()
  "Run tests"
  (format 't "~&Running AX.25 Test Suite:")
  (let ((data (string-to-bytes "A")))
    (do-test "CRC16 Single Character: A" '#xA3F5 (crc16 data))
    (do-test "FCS   Single Character: A" '(#xF5 #xA3) (fcs data)))
  (let ((data (string-to-bytes "Andrew")))
    (do-test "CRC16 Multiple Characters: Andrew" '#xF3C3 (crc16 data))
    (do-test "FCS   Multiple Characters: Andrew" '(#xC3 #xF3) (fcs data)))
  (do-test "Bit Stuffing" '((204 23 248) 2 2 1) (stuff-bits '(#b11001100 #b00001111 #b11111100)))
  (do-test "Bit Stuffing - Continued" '((#xBE) 4 3 1) (let ((o (stuff-bits '(#b11001100 #b00001111 #b11111100))))
                                                              (stuff-bits '(#b10011111) nil nil (cadr o) (caddr o) (cadddr o))))
  (do-test "Add Flag" '(126 0 0) (add-flag))
  (do-test "Add Flag - Continued" '(239 7 4) (add-flag '#b00001111 4))
  (format 't "~&Finished AX.25 Test Suite"))


