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

;; Functions for writing frames to a collection of bytes

(in-package :ax25)

(defun frame-header-to-bytes (frame)
  "Converts an AX.25 frame header struct to a sequence of bytes."
  (flet ((address (s) (string-to-bytes (format 'nil "~6a" s) :shift 1 ))
         (ssid (s &optional final)
           (list (if final
                     (logior (ash s 1) '#b11100001)
                     (logior (ash s 1) '#b11100000)))))
    (concatenate 'list
                 (address (frame-source frame))
                 (ssid (frame-source-ssid frame))
                 (address (frame-destination frame))
                 (ssid (frame-destination-ssid frame) 't)
                 (list                       
                  (frame-control frame)
                  (frame-pid frame)))))

(defun frame-to-bytes (frame &key (fcs 't))
  "Returns an AX.25 frame as a byte sequence.  If :fcs is nil, the FCS will not be written."
  (let* ((data (if (stringp (frame-data frame))
                   (string-to-bytes (frame-data frame))
                   (frame-data frame)))
         (frame-bytes (concatenate 'list
                                   (frame-header-to-bytes frame)
                                   data))
         (fcs-bytes (if fcs (fcs frame-bytes) nil)))
    (concatenate 'list frame-bytes fcs-bytes)))

(defun write-frame (frame out &key (fcs 't))
  "Write a binary representation of the given AX.25 frame to the given output stream."
  (write-sequence (frame-to-bytes frame :fcs fcs) out))

(defun write-frame-to-file (frame filename &key (fcs 't) (if-exists :supersede))
  "Write an AX.25 frame to a file."
  (with-open-file (out filename
                       :direction :output
                       :if-exists if-exists
                       :element-type '(unsigned-byte 8))
    (write-frame frame out :fcs fcs)))
