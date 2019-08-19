# AX.25 Utilities for Common Lisp

This library includes utilities for creating and parsing AX.25 frames.
AX.25 is used for amateur radio communication, especially with small satellites
such as cubesats.

Usage:
```lisp
(require :ax25)

(ax25:write-frame-to-file
  (ax25:make-frame
    :source "EARTH"
    :destination "SPACE"
    :data (ax25:string-to-bytes "Ground Control to Major Tom"))
  "test.bin")
```
