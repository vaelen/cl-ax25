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

(defun bytes-to-frame (bytes)
  "Reads an AX.25 frame from the given byte array."
  (flet ((read-address (bytes)
           (bytes-to-string bytes :shift -1))
         (read-ssid (b) (ldb (byte 4 1) b)))
    (let ((frame (make-frame)))
      (setf (frame-source frame) (read-address (subseq bytes 0 6)))
      (setf (frame-source-ssid frame) (read-ssid (first (subseq bytes 6 7))))
      (setf (frame-destination frame) (read-address (subseq bytes 7 13)))
      ;; TODO: Support more than two address fields
      (setf (frame-destination-ssid frame) (read-ssid (first (subseq bytes 13 14))))
      ;; TODO: Parse Control and PID
      (setf (frame-data frame) (subseq bytes 16))
      frame)))

(defun read-frames (in frame-handler &optional failed-frame-handler)
  "Reads frames from the input stream and calls frame-handler for each frame read."
  (let ((new-byte 0)
        (new-bit 0)
        (last-8-bits 0)
        (in-frame nil)
        (result '()))
    (flet ((push-byte ()
             ;; Push new byte onto results
             (push new-byte result)
             ;; Reset values
             (setf new-byte 0)
             (setf new-bit 0))
           (call-handler ()
             (when (rest result)
               (let*
                   ;; Unstuff bits
                   ((unstuffed (reverse (unstuff-bits (reverse (rest result)))))
                    ;; Get FCS
                    (fcs-bytes (list (second unstuffed) (first unstuffed)))
                    ;; Get Frame
                    (frame-bytes (reverse (rest (rest unstuffed)))))
                 (if (equal fcs-bytes (fcs frame-bytes))
                     (funcall frame-handler (bytes-to-frame frame-bytes))
                     (if failed-frame-handler
                         (funcall failed-frame-handler (bytes-to-frame frame-bytes)))))
               ;; Clear results
               (setf result '()))))
      (do ((byte (read-byte in nil)
                 (read-byte in nil)))
          ((not byte))
        (when byte
          (loop for bit upto 7 do
            (progn
              (when in-frame
                ;; Copy next bit into result byte
                (setf (ldb (byte 1 new-bit) new-byte) (ldb (byte 1 bit) byte))
                (setf new-bit (+ 1 new-bit))
                (if (= 8 new-bit) (push-byte)))
              ;; Shift 8 bit window
              (setf last-8-bits (ash last-8-bits -1))
              ;; Add current bit to 8 bit window
              (setf (ldb (byte 1 7) last-8-bits) (ldb (byte 1 bit) byte))
              ;; Check 8 bit window for flag. (this works because the flag is symetrical)
              (when (= *flag* last-8-bits)
                (setf in-frame (not in-frame))
                (if (not (= 0 new-bit)) (push-byte))
                (call-handler)))))))))

(defun read-frames-from-file (filename frame-handler &optional failed-frame-handler)
  "Read AX.25 frames from a file and call frame-handler for each frame"
  (with-open-file (in filename
                       :direction :input
                       :element-type '(unsigned-byte 8))
    (read-frames in frame-handler failed-frame-handler)))

(defun print-frame-handler (frame)
  "This frame handler prints out the frame contents to standard out"
  (format 't "~&~a" frame))
