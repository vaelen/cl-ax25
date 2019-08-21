# AX.25 Utilities for Common Lisp

This library includes utilities for creating and parsing AX.25 frames.
AX.25 is used for amateur radio communication, especially with small satellites
such as cubesats.

Usage:
```lisp
(require :ax25)

(ax25:write-frames-to-file
  (list
    (ax25:make-frame
      :source "GRDCTL"
      :destination "MAJTOM"
      :data (ax25:string-to-bytes "Ground Control to Major Tom")))
  "test.bin")

(ax25:read-frames-from-file "test.bin" #'ax25:print-frame-handler)
```
