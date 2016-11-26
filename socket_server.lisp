(require :sb-bsd-sockets)
(in-package :sb-thread)
(rename-package :sb-bsd-sockets :s)
(rename-package :sb-thread :t)

(defparameter *localhost* '(127 0 0 1))

(defun server-socket (port)
  "Creates a server socket, and sets it to the listen state. Non-blocking sockets are
   tricky in LISP, so we will survive with this plus a signal handler"
  (let ((socket (make-instance 's:inet-socket :type :stream :protocol :tcp)))
    (s:socket-bind socket *localhost* port)
    (s:socket-listen socket 100)
    (format t "Server up ~a" socket)
    socket))

(defun read-socket (so)
  "Takes a socket connected to a client, and reads from it"
  (unwind-protect
       (let ((stream (s:socket-make-stream so :output t :input t)))
         (format stream "Connected ~a~%" so)
         (finish-output stream)
         (let ((line (read-line stream)))
           (loop while (not (string= (subseq line 0 (length line)) "exit")) do
                (format t "~s~s~%" line so)
                (setf line (read-line stream)))))
    (s:socket-close so)))

(defun accept-stream (socket)
  "Accepts one connection and spans a new thread to handle it asynchronously"
  (let ((so (s:socket-accept socket)))
    (t:make-thread (lambda () (read-socket so)))))

(defun server (port)
  (let ((socket (server-socket port)))
    (unwind-protect
         (loop
            (accept-stream socket))
      (format t "About to close socket")
      (s:socket-close socket))))

(server 8080)
