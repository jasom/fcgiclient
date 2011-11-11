;;;; fcgiclient.lisp

(declaim (optimize (speed 3)))

(in-package #:fcgiclient)

(defconstant +fcgi-listensock-fileno+   0)

(defconstant +fcgi-header-length+   8)

(defconstant +fcgi-version-1+   1)

(defconstant +fcgi-begin-request+   1)
(defconstant +fcgi-abort-request+   2)
(defconstant +fcgi-end-request+   3)
(defconstant +fcgi-params+   4)
(defconstant +fcgi-stdin+   5)
(defconstant +fcgi-stdout+   6)
(defconstant +fcgi-stderr+   7)
(defconstant +fcgi-data+   8)
(defconstant +fcgi-get-values+   9)
(defconstant +fcgi-get-values-result+   10)
(defconstant +fcgi-unknown-type+   11)
(defconstant +fcgi-maxtype+   +fcgi-unknown-type+)

(defconstant +fcgi-null-request-id+   0)

(defconstant +fcgi-keep-conn+   1)

(defconstant +fcgi-responder+   1)
(defconstant +fcgi-authorizer+   2)
(defconstant +fcgi-filter+   3)

(defconstant +fcgi-request-complete+   0)
(defconstant +fcgi-cant-mpx-conn+   1)
(defconstant +fcgi-overloaded+   2)
(defconstant +fcgi-unknown-role+   3)

(defparameter +fcgi-header+ '(1 1 2 2 1))
(defparameter +fcgi-begin-request-body+ '(2 1))
(defparameter +fcgi-end-request-body+ '(4 1))
(ql:quickload :iolib.sockets)


(defstruct fcgi-record
    (version +fcgi-version-1+ :type (unsigned-byte 8))
    (type 0 :type (unsigned-byte 8))
    (request-id 0 :type (unsigned-byte 16))
    (content-length 0 :type (unsigned-byte 16))
    (padding-length 0 :type (unsigned-byte 8))
    (payload (coerce #() '(simple-array (unsigned-byte 8) (*)))
             :type (simple-array (unsigned-byte 8) (*))))

(defun decode-bytes (buffer start n)
  (loop for i from start to (+ start (1- n))
        for result = (aref buffer i) then (+ (ash result 8) (aref buffer i))
        finally (return result)))

(defun decode-struct (spec buffer &optional (start 0))
  (when spec
    (cons
    (decode-bytes buffer start (car spec))
    (decode-struct (cdr spec) buffer (+ start (car spec))))))

(defun encode-bytes (buffer start v n)
  (loop for i from 0 to (1- n)
       do (setf (aref buffer (+ i start)) (logand 255 (ash v (* -8 (1- (- n i))))))))

(defun fcgi-record-to-bytes (record)
  (setf (fcgi-record-padding-length record) 
	(mod (- 8
		(mod (fcgi-record-content-length record) 8)) 8))
  (let*  ((size (+ +fcgi-header-length+ (fcgi-record-content-length record)
		   (fcgi-record-padding-length record)))
          (buffer (make-array (list size) :element-type '(unsigned-byte 8)
                              :initial-element 0)))
    (encode-bytes buffer 0 (fcgi-record-version record) 1)
    (encode-bytes buffer 1 (fcgi-record-type record) 1)
    (encode-bytes buffer 2 (fcgi-record-request-id record) 2)
    (encode-bytes buffer 4 (fcgi-record-content-length record) 2)
    (encode-bytes buffer 6 (fcgi-record-padding-length record) 1)
    (encode-bytes buffer 7 0 1)
    (replace buffer (fcgi-record-payload record) :start1 8)
    buffer))

(defconstant +fcgi-begin-request-body-length+ (+ 2 1 5))
(defconstant +fcgi-end-request-body-length+  (+ 4 1 3))
(defconstant +fcgi-header-length+  (+ 1 1 2 2 1 1))

(defun recvall (socket bytes)
  (declare (type fixnum bytes))
  ;(print :recv)
  (let ((buffer (make-array (list bytes) :element-type '(unsigned-byte 8)))
	(sofar 0))
    (loop while (< sofar bytes)
       do (iomux:wait-until-fd-ready (iolib.sockets:socket-os-fd socket)
				     :input nil)
       do (multiple-value-bind (_ bytes-read)
	      (iolib.sockets:receive-from socket :start sofar :buffer buffer)
	    (incf sofar bytes-read)))
    ;(let ((*print-radix* 16))
      ;(if (> (length buffer) 32)
        ;(princ (subseq buffer 0 32))
        ;(princ buffer)))
    (values buffer sofar)))

(defun sendall (socket bytes)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  ;(print :send)
  ;(let ((*print-radix* 16)) (print bytes))
  (let ((sofar 0))
    (declare (type fixnum sofar))
    (loop while (< sofar (length bytes))
          do (incf sofar (iolib.sockets:send-to socket bytes :start sofar)))))

(defun encode-pair (key value)
  (let ((key (if (typep key 'string) (babel:string-to-octets key) key))
        (value (if (typep value 'string) (babel:string-to-octets value) value)))
    (declare (type (simple-array (unsigned-byte 8) (*)) key value))
    (concatenate '(simple-array (unsigned-byte 8) (*))
                 (if (< (length key) 128)
                   (list (length key))
                   (encode-bytes (make-array '(4) :element-type '(unsigned-byte 8))
                                 0 (logior #x80000000 (length key)) 4))
                 (if (< (length value) 128)
                   (list (length value))
                   (encode-bytes (make-array '(4) :element-type '(unsigned-byte 8))
                                 0 (logior #x80000000 (length value)) 4))
                 key
                 value)))

(defun make-fcgi-params (params id)
  (let ((payload 
          (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                 (loop for (key . value) in params
                       collect (encode-pair key value)))))
    (declare (type (simple-array (unsigned-byte 8)) payload))
    (make-fcgi-record
      :type +fcgi-params+
      :request-id id
      :payload payload
      :content-length (length payload))))

(defun read-record (socket)
  (multiple-value-bind (header n) (recvall socket +fcgi-header-length+)
    (declare (type fixnum n)
             (type (simple-array (unsigned-byte 8) (*)) header))
    (assert (= n +fcgi-header-length+))
    (destructuring-bind (version type request-id content-length padding-length)
      (decode-struct +fcgi-header+ header)
      (multiple-value-bind (content n) (recvall socket content-length)
        (assert (= n content-length))
        (recvall socket padding-length)
        (make-fcgi-record
          :version version
          :type type
          :request-id request-id
          :content-length content-length
          :padding-length padding-length
          :payload content)))))

(defun do-request (sock env bod &key (id 1) (keep nil))
  (sendall sock 
           (fcgi-record-to-bytes (make-fcgi-record
             :type +fcgi-begin-request+
             :request-id id
             :payload (coerce (list 0 +fcgi-responder+ (if keep +fcgi-keep-conn+ 0)) '(simple-array (unsigned-byte 8) (*)))
             :content-length +fcgi-begin-request-body-length+)))
  (sendall sock (fcgi-record-to-bytes (make-fcgi-params env id)))
  (sendall sock (fcgi-record-to-bytes (make-fcgi-params nil id)))
  ;(when  (> (length bod) 0)
  (sendall sock
           (fcgi-record-to-bytes (make-fcgi-record
                                   :type +fcgi-stdin+
                                   :request-id id
                                   :payload bod
                                   :content-length (length bod))))
  ;)
  ;(sendall sock (fcgi-record-to-bytes (make-fcgi-record
             ;:type +fcgi-data+
             ;:request-id id)))

  (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                       (loop for record = (read-record sock)
                             while (not (= (fcgi-record-type record) +fcgi-end-request+))
                             when (= (fcgi-record-type record) +fcgi-stdout+)
                             collect (fcgi-record-payload record))))
(defun hello-world ()
  (map 'string #'code-char (iolib.sockets:with-open-socket (s :remote-host "localhost" :remote-port 9000       
				     :address-family :internet             
				     :type :stream                         
				     :connect :active)                     
    (do-request  s 
      '(("SCRIPT_FILENAME"."foo.php")         
      ("REQUEST_METHOD"."GET")                   
      ("SCRIPT_NAME"."/foo.php")                 
      ("REQUEST_URI"."/foo.php")                 
      ;("GATEWAY_INTERFACE"."CGI/1.1")            
      ("CONTENT_TYPE"."")                        
      ("CONTENT_LENGTH"."0")                     
      ("DOCUMENT_ROOT"."/"))
      (coerce #() '(simple-array (unsigned-byte 8) (*)))))))

(defun hello-world-keep-open ()
  (iolib.sockets:with-open-socket (s :remote-host "localhost" :remote-port 9000       
                                     :address-family :internet             
                                     :type :stream                         
                                     :connect :active)                     
    (loop
      (values
        (map 'string #'code-char
             (do-request  s 
                          '(("SCRIPT_FILENAME"."foo.php")         
                            ("REQUEST_METHOD"."GET")                   
                            ("SCRIPT_NAME"."/foo.php")                 
                            ("REQUEST_URI"."/foo.php")                 
                            ;("GATEWAY_INTERFACE"."CGI/1.1")            
                            ("CONTENT_TYPE"."")                        
                            ("CONTENT_LENGTH"."0")                     
                            ("DOCUMENT_ROOT"."/"))
                          (coerce #() '(simple-array (unsigned-byte 8) (*)))
                          :keep t))))))
