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
          (loop
            ;(print "X")
            (let ((req (recv)))
              ;(print req)
              (when (not (request-disconnectp req))
                (iolib.sockets:with-open-socket (s
                                        :remote-host fcgi-host
                                        :remote-port fcgi-port
                                        :type :stream
                                        :connect :active)
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
                    (reply req (subseq response body-start)))))
                (when (request-closep req) (reply-close req)))))))

(defstruct (fcgi-connection (:conc-name fcc-))
  (socket nil)
  (sofar () :type list)
  (pollitem nil :type zmq:pollitem)
  (req nil))


(defun m2fcgi-fancy (m2-in m2-out m2-id fcgi-host fcgi-port fcgi-script
                           &optional (num-conns 35)
                           &aux (fcgi-id 1)
                           (active-conns (make-hash-table)))
  (flet ((c2fcgi ()
          (iolib.sockets:make-socket
                                           :address-family :internet
                                           :type :stream
                                           :connect :active
                                           :remote-host fcgi-host
                                           :remote-port fcgi-port)))

  ;(let ((sockets
          ;(loop repeat num-conns collect (iolib.sockets:make-socket
                                           ;:address-family :internet
                                           ;:type :stream
                                           ;:connect :active
                                           ;:remote-host fcgi-host
                                           ;:remote-port fcgi-port))))

  (with-connection (m2c m2-id m2-in m2-out)
    (let ((m2poll (make-instance 'zmq:pollitem
                                 :socket (slot-value *current-connection*
                                                                  'mymongrel2::reqs)
                                 :events zmq:pollin)))
      (loop
        (setf (zmq:pollitem-events m2poll) zmq:pollin)
        (let* ((pollitems (loop for val being the hash-values of active-conns
                                for p = (fcc-pollitem val)
                                do (setf (zmq:pollitem-events p) zmq:pollin)
                                collect p))
               (pollingm2 (< (hash-table-count active-conns) num-conns))
               (pollitems (if pollingm2 (cons m2poll pollitems) pollitems))
               (revents (zmq:poll pollitems)))
          (when pollingm2
            (pop pollitems)
            (when (= (pop revents) zmq:pollin)
              (let ((req (recv)))
                (when (not (request-disconnectp req))
                  (let
                       ((env (make-fcgi-environment req fcgi-script))
                        (s (c2fcgi)))
                       (setf (gethash (iolib.sockets:socket-os-fd s) active-conns)
                                      (make-fcgi-connection
                                        :socket s
                                        :sofar ()
                                        :pollitem (make-instance 'zmq:pollitem
                                                                 :fd (iolib.sockets:socket-os-fd s)
                                                                 :events zmq:pollin)
                                        :req req))
                       (fcgiclient::start-request s env (request-body req)
                                             :id fcgi-id
                                             :keep t))))))
          (loop for ev in revents
                for p in pollitems
                for fd = (zmq:pollitem-fd p)
                for fcc = (gethash fd active-conns)
                do (multiple-value-bind (sofar done)
                     (fcgiclient::do-some (fcc-socket fcc) (fcc-sofar fcc))
                     (if (not done)
                       (setf (fcc-sofar fcc) sofar)
                       (let ((response (fcgiclient::get-result sofar)))
                         (multiple-value-bind (header body-start) (fix-fcgi-headers response)
                           (reply (fcc-req fcc) header)
                           (reply (fcc-req fcc) (subseq response body-start)))
                         (when (request-closep (fcc-req fcc)) (reply-close (fcc-req fcc)))
                         (remhash fd active-conns)
                         (close (fcc-socket fcc))))))))))))




(defun test () 
  (m2fcgi "tcp://127.0.0.1:9993" "tcp://127.0.0.1:9992"
          (format nil "~X" (random (ash 1 64)))
          "localhost" 9000 "foo.php"))
(defun test-fancy () 
  (m2fcgi-fancy "tcp://127.0.0.1:9993" "tcp://127.0.0.1:9992"
          (format nil "~X" (random (ash 1 64)))
          "localhost" 9000 "foo.php"))
(setq *random-state* (make-random-state t))
