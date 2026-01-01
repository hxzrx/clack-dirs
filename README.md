Example

```lisp
(ql:quickload '(:clack :ningle :clack-dirs))

(defparameter *app* nil)
(defvar *server-handler* nil
  "Clack server instance.")

(defun start-server (&key (port 5000))
  (setf *app* (make-instance 'ningle:<app>))
  (setf *server-handler*
        (clack:clackup *app* :port port)))

(defun stop-server ()
  (when *server-handler*
    (clack:stop *server-handler*)
    (setf *server-handler* nil)
    (setf *app* nil)
    (format t "Ningle File Server stopped.~%")))

(start-server)

;; wget http://localhost:5000/api/v1/static
(clack-dirs:serve-directory *app* "~/tmp/" "/api/v1/static")
```
