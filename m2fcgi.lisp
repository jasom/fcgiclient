(ql:quickload :alexandria)
(ql:quickload :mymongrel2)
(ql:quickload :fcgiclient)

(use-package :mymongrel2)
(use-package :alexandria)
(use-package :fcgiclient)

(declaim (optimize (speed 3)))

(defparameter +CRNL+ #.(babel:string-to-octets (concatenate 'simple-string '(#\Return #\Newline))))

(defmacro apush (key value alist)
  `(push (cons ,key ,value) ,alist))

(defun make-fcgi-key (str)
  (map 'simple-string
       (lambda (x) (if (eql x #\-) #\_ (char-upcase x))) str))


(defun make-fcgi-environment (req script-name)
  (let ((env nil))
    (loop for (key . value) in (request-headers req)
          when (notany #'upper-case-p (string key))
          do (apush (concatenate 'simple-string "HTTP_" (make-fcgi-key (string key))) value env)
          do (case (if (keywordp key) key (make-keyword key))
               (:|content-type| (apush  "CONTENT_TYPE" value env))
               (:|QUERY| (apush "QUERY_STRING" value env))
               (:|x-forwarded-for| (apush "REMOTE_ADDR" value env))
               (:|METHOD| (apush "REQUEST_METHOD" value env))
               (:|host| (apush "SERVER_NAME" value env))
               (:|VERSION| (apush "SERVER_PROTOCOL" value env))
               ))
    (apush "GATEWAY_INTERFACE" #.(babel:string-to-octets "CGI/1.1") env)
    (apush "SCRIPT_FILENAME" (babel:string-to-octets script-name) env)
    (apush "SERVER_SOFTWARE" (babel:string-to-octets "m2fcgi") env)
    (apush #.(babel:string-to-octets "CONTENT_LENGTH")
                      (babel:string-to-octets
                        (format nil "~D" (length (request-body req)))) env)
    env))

    ;TODO PATH_INFO SCRIPT_NAME SERVER_PORT REQUEST_URI
               

(defun fix-fcgi-headers (resp)
  (declare (type (simple-array (unsigned-byte 8) (*)) resp))
  (multiple-value-bind (headers body-start status)
         (loop
           with status of-type (simple-array (unsigned-byte 8) (*)) = #.(babel:string-to-octets (format nil "200 OK~C~C" #\Return #\Newline))
           for pos of-type fixnum = 0 then newpos
           for newpos of-type fixnum = (1+ (position #.(char-code #\Newline) resp :start pos))
           for str of-type (simple-array (unsigned-byte 8) (*)) = (subseq resp pos newpos)
           ;do (print str)
           when (<= (length str) 2)
           collect +CRNL+ into retval
           ;TODO fix line-ending on status
           when (and (> (length str) #.(length "Status:"))
                     (equalp (subseq str 0 7) #.(babel:string-to-octets "Status:")))
           do (setq status (subseq str (1+ (position #.(char-code #\:) str))))
           until (<= (length str) 2)
           collect (if (eql #.(char-code #\Return) (aref str (- (length str) 2))) str
                     (concatenate '(simple-array (unsigned-byte 8) (*)) (subseq resp pos (1- newpos)) +CRNL+)) into retval
           finally (return (values retval newpos status)))
  (values (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                 #.(babel:string-to-octets "HTTP/1.1 ")
                 status
                 (babel:string-to-octets (format nil "content-length: ~D" (- (length resp) body-start)))
                           +CRNL+
                 headers)
          body-start)))


(defun m2fcgi (m2-in m2-out m2-id fcgi-host fcgi-port fcgi-script &aux (fcgi-id 1))
  (with-connection (m2c m2-id m2-in m2-out)
      (iolib.sockets:with-open-socket (s
                                        :remote-host fcgi-host
                                        :remote-port fcgi-port
                                        :type :stream
                                        :connect :active)
          (loop
            ;(print "X")
            (let ((req (recv)))
              ;(print req)
              (when (not (request-disconnectp req))
                (let* ((env (make-fcgi-environment req fcgi-script))
                       (response (do-request s env (request-body req)
                                             :id fcgi-id
                                             :keep t)))
                  (declare (type list env)
                           (type (simple-array (unsigned-byte 8) (*)) response))
                  ;(print (request-headers req))
                  ;(print response)
                  (multiple-value-bind (header body-start) (fix-fcgi-headers response)
                    (reply req header)
                    (reply req (subseq response body-start))))
                (when (request-closep req) (reply-close req))))
            ;(incf fcgi-id)
            ))))

(require :sb-sprof)
(defun m2fcgi-profile (m2-in m2-out m2-id fcgi-host fcgi-port fcgi-script &aux (fcgi-id 1))
  (with-connection (m2c m2-id m2-in m2-out)
      (iolib.sockets:with-open-socket (s
                                        :remote-host fcgi-host
                                        :remote-port fcgi-port
                                        :type :stream
                                        :connect :active)
    (sb-sprof:with-profiling (:sample-interval 0.01 :max-samples 10000 :report :graph :loop t :reset t)
          (dotimes (i 100)
            ;(print "X")
            (let ((req (recv)))
              ;(print req)
              (when (not (request-disconnectp req))
                (let* ((env (make-fcgi-environment req fcgi-script))
                       (response (do-request s env (request-body req)
                                             :id fcgi-id
                                             :keep t)))
                  (declare (type list env)
                           (type (simple-array (unsigned-byte 8) (*)) response))
                  ;(print (request-headers req))
                  ;(print response)
                  (multiple-value-bind (header body-start) (fix-fcgi-headers response)
                    (reply req header)
                    (reply req (subseq response body-start))))
                (when (request-closep req) (reply-close req))))
            ;(incf fcgi-id)
            )))))



(defun profile-it () 
  (m2fcgi-profile "tcp://127.0.0.1:9993" "tcp://127.0.0.1:9992"
          (format nil "~X" (random (ash 1 64)))
          "localhost" 9000 "foo.php"))

(defun test () 
  (m2fcgi "tcp://127.0.0.1:9993" "tcp://127.0.0.1:9992"
          (format nil "~X" (random (ash 1 64)))
          "localhost" 9000 "foo.php"))

(setq *random-state* (make-random-state t))
