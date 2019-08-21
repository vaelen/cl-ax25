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
  "Returns an AX.25 frame as a byte sequence.  
   If :fcs is nil, the FCS will not be written."
  (let* ((data (if (stringp (frame-data frame))
                   (string-to-bytes (frame-data frame))
                   (frame-data frame)))
         (frame-bytes (concatenate 'list
                                   (frame-header-to-bytes frame)
                                   data))
         (fcs-bytes (if fcs (fcs frame-bytes) nil)))
    (concatenate 'list frame-bytes fcs-bytes)))

(defun stuff-bits (bytes &optional (current-byte nil) (current-bit nil) (new-byte 0) (new-bit 0) (one-count 0) (result '()))
  "Add LSB bit stuffing to the given byte sequence. 
   Returns the full bytes, the last byte, the number of bits in the last byte, and the one count."
  (cond
    ((and (not current-bit) (not bytes))
     ;; No input, so return an empty list
     '())
    ((not current-bit)
     ;; Start recursion
     (stuff-bits (rest bytes) (first bytes) 0 new-byte new-bit one-count result))
    ((not current-byte)
     ;; Finished
     (list (reverse result) new-byte new-bit one-count))
    ((= current-bit 8)
     ;; Finished with the current byte
     (stuff-bits (rest bytes) (first bytes) 0 new-byte new-bit one-count result))
    ((= new-bit 8)
     ;; Finished with the new byte
     (push new-byte result)
     (stuff-bits bytes current-byte current-bit 0 0 one-count result))
    ((= one-count 5)
     ;; Need to stuff in a zero in there
     (setf (ldb (byte 1 new-bit) new-byte) 0)
     (stuff-bits bytes current-byte current-bit new-byte (+ 1 new-bit) 0 result))
    ((= (ldb (byte 1 current-bit) current-byte) 0)
     ;; Next bit is a zero, add it and reset the one count
     (setf (ldb (byte 1 new-bit) new-byte) 0)
     (stuff-bits bytes current-byte (+ 1 current-bit) new-byte (+ 1 new-bit) 0 result))
    ('t
     ;; Next bit is a one
     (setf (ldb (byte 1 new-bit) new-byte) 1)
     (stuff-bits bytes current-byte (+ 1 current-bit) new-byte (+ 1 new-bit) (+ 1 one-count) result))))

(defun add-flag (&optional (start-byte 0) (start-bit 0))
  "Adds an HLDC/AX.25 flag to the given start byte at the given start bit.
   Returns a full byte, a remaining byte, and the number of bits in the remaining byte."
  (setf (ldb (byte 8 start-bit) start-byte) *flag*)
  (list (ldb (byte 8 0) start-byte) (ldb (byte 8 8) start-byte) start-bit))

(defun write-raw-frame (frame out &key (fcs 't))
  "Write a raw AX.25 frame to an output stream without flags or bit stuffing."
  (write-sequence (frame-to-bytes frame :fcs fcs) out))

(defun write-raw-frame-to-file (frame filename &key (fcs 't) (if-exists :supersede))
  "Write a raw AX.25 frames to a file without flags or bit stuffing."
  (with-open-file (out filename
                       :direction :output
                       :if-exists if-exists
                       :element-type '(unsigned-byte 8))
    (write-raw-frame frame out :fcs fcs)))

(defun write-frames (frames out)
  "Write the frames to the output stream, including flags and bit stuffing."
  (if frames
      (let ((result '(nil 0 0)))
        (write-byte *flag* out)
        (loop for frame in frames do
          (progn
            (setf result (stuff-bits (frame-to-bytes frame) nil nil (cadr result) (caddr result)))
            (write-sequence (car result) out)
            (setf result (add-flag (cadr result) (caddr result)))
            (write-byte (car result) out)))
        (if (not (= 0 (caddr result)))
            (write-byte (cadr result) out)))))

(defun write-frames-to-file (frames filename &key (if-exists :supersede))
  "Write AX.25 frames to a file, including flags and bit stuffing."
  (with-open-file (out filename
                       :direction :output
                       :if-exists if-exists
                       :element-type '(unsigned-byte 8))
    (write-frames frames out)))

