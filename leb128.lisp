(defpackage :leb128
  (:use :cl)
  (:export :encode-signed :decode-signed
           :encode-unsigned :decode-unsigned))
(in-package :leb128)

(defmethod encode-signed (i)
  "Encode an integer of arbitrary length into a leb128 unsigned-8 buffer"
  (let ((more t) (curr) (in 0) (ret (make-array
                                     (if (>= i 0)
                                         (if (= (logand #x40 (mod i 128)) 64)
                                             (+ 1 (ceiling (/ (log (+ 2 (abs i)) 2) 7)))
                                             (ceiling (/ (log (+ (abs i) 2) 2) 7)))
                                         (if (= (logand #x40 (mod i 128)) 64)
                                             (ceiling (/ (log (+ 2 (abs i))) 7))
                                             (+ 1 (ceiling (/ (log (+ 2 (abs i))) 7)))))
                                     :element-type '(unsigned-byte 8)))) ;(neg (< i 0))
    (loop while more do
         (setf curr (logand i #x7f))
         (setf i (ash i -7))
         (if (or (and (= i 0)  (= (logand curr #x40) 0))
                 (and (= i -1) (= (logand curr #x40) 64)))
             (setf more nil)
             (setf curr (logior curr #x80)))
         (setf (aref ret in) curr)
         (incf in))
    ret))

(defmethod decode-signed ((s stream) &key (start 0))
  "decode signed integer from stream. Returns (values decoded-integer
num-bytes-consumed)"
  (when (not (= start 0))
    (loop for i from 0 upto start do (read-byte s)))
  (let ((result 0) (shift 0) (curr) (counter 0))
    (loop do 
         (setf curr (read-byte s))
         (setf result (logior result (ash (logand curr #x7f) shift)))
         (setf shift (+ 7 shift))
         (incf counter)
         (when (= 0 (logand curr #x80))
           (if (= 64 (logand curr #x40))
               (return-from decode-signed (values (logior result (ash (lognot 0) shift)) counter))
               (return-from decode-signed (values result counter)))))))

(defmethod decode-signed ((buf array) &key (start 0))
  "decode signed integer from buffer. Returns (values decoded-integer
num-bytes-consumed)"
  (let ((result 0) (shift 0) (curr) (counter 0))
    (loop do 
         (setf curr (aref buf start))
         (setf start (+ 1 start))
         (setf result (logior result (ash (logand curr #x7f) shift)))
         (setf shift (+ 7 shift))
         (incf counter)
         (when (= 0 (logand curr #x80))
           (if (= 64 (logand curr #x40))
               (return-from decode-signed (values (logior result (ash (lognot 0) shift)) counter))
               (return-from decode-signed (values result counter)))))))

(defmethod encode-unsigned (i)
  "Encode an arbitrarily large unsigned integer in leb128"
  (declare (type unsigned-byte i))
  (let ((more t) (curr) (in 0) (ret (make-array
                                     (ceiling  (/ (log (+ i 1) 2) 7))
                                     :element-type '(unsigned-byte 8)))) ;(neg (< i 0))
    (loop while more do
         (setf curr (logand i #x7f))
         (setf i (ash i -7))
         (if (= i 0)
             (setf more nil)
             (setf curr (logior curr #x80)))
         (setf (aref ret in) curr)
         (incf in))
    ret))

(defmethod decode-unsigned ((s stream) &key (start 0))
  "Decode an arbitrarily large unsigned integer from stream. Skip
_start_ number bytes. Return (values integer-decoded
num-bytes-consumed)"
  (when (not (= start 0))
    (loop for i from 0 upto start do (read-byte s)))
  (let ((result 0) (shift 0) (curr) (counter 0))
    (loop do 
         (setf curr (read-byte s))
         (setf result (logior result (ash (logand curr #x7f) shift)))
         (setf shift (+ 7 shift))
         (incf counter)
         (when (= 0 (logand curr #x80))
           (return-from decode-unsigned (values result counter))))))

(defmethod decode-unsigned ((buf array) &key (start 0))
  "decode signed integer from buffer. Returns (values decoded-integer
num-bytes-consumed)"
  (let ((result 0) (shift 0) (curr) (counter 0))
    (loop do 
         (setf curr (aref buf start))
         (setf start (+ 1 start))
         (setf result (logior result (ash (logand curr #x7f) shift)))
         (setf shift (+ 7 shift))
         (incf counter)
         (when (= 0 (logand curr #x80))
           (return-from decode-unsigned (values result counter))))))
