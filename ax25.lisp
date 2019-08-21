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

;; AX.25 Functions

(defpackage :ax25
  (:use :common-lisp)
  (:export :frame
           :make-frame
           :*example-frame*
           :crc16
           :fcs
           :frame-to-bytes
           :string-to-bytes
           :add-flag
           :stuff-bits
           :unstuff-bits
           :write-raw-frame
           :write-frames
           :write-raw-frame-to-file
           :write-frames-to-file
           :test-suite))

(in-package :ax25)

(defparameter *flag* '#b01111110)

(defun string-to-bytes (s &key (shift 0))
  "Converts a string to a byte sequence.  Can bit-shift at the same time."
  (flet ((char-to-byte (c) (ash (char-code c) shift)))
    (map 'list #'char-to-byte s)))

(defstruct frame
  "Represents an AX.25 frame."
  (source "SOURCE")      ; Source Address
  (source-ssid 0)        ; Source SSID
  (destination "DEST")   ; Destination Address
  (destination-ssid 0)   ; Destination SSID
  (control #b00000011)   ; Unnumbered Information Frame
  (pid #b11110000)       ; No Protocol Information
  (data nil)             ; Data should be in bytes
  (fcs #x0000))          ; FCS will be calculated

(defparameter *example-frame*
  (make-frame
   :source "GRDCTL"
   :destination "MAJTOM"
   :data (string-to-bytes "Ground Control to Major Tom.")))
