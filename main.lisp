;;;; Serve directories and files.

(defpackage #:clack-dirs
  (:use #:cl)
  (:import-from
   #:local-time
   #:format-timestring
   #:universal-to-timestamp)
  (:import-from
   #:str
   #:split)
  (:import-from
   #:quri
   #:url-encode
   #:url-decode)
  (:import-from
   #:cl-fad
   #:directory-exists-p
   #:directory-pathname-p
   #:list-directory
   #:merge-pathnames-as-directory
   #:pathname-as-directory
   #:pathname-equal
   #:pathname-parent-directory)
  (:import-from
   #:trivial-mimes
   #:mime)
  (:import-from
   #:trivial-file-size
   #:file-size-in-octets)
  (:export
   #:verify-auth
   #:serve-directory))

(in-package #:clack-dirs)


(defun directory-files (dir)
  "Get the pathnames of all files and dirs in `dir'."
  (when (directory-exists-p dir)
    (sort (list-directory dir)
          (lambda (a b)
            (let ((dir-a (directory-pathname-p a))
                  (dir-b (directory-pathname-p b)))
              (cond
                ((and dir-a (not dir-b)) t)    ; dirs first
                ((and (not dir-a) dir-b) nil)
                ;; sort by name
                (t (string< (file-namestring a)
                            (file-namestring b)))))))))

(defun mime-type-from-pathname (pathname)
  "Get the mime-type according to the file ext."
  (mime pathname))

(defun directory-namestring% (pathname)
  "Return the dir name from the full pathname."
  (car (last (split "\/" (string-trim '(#\/) (directory-namestring pathname))))))

(defun encode-path-for-url (path)
  (let* ((segments (split "/" path))
         (encoded (mapcar #'url-encode segments)))
    (format nil "~{~A~^/~}" encoded)))

(defun render-directory-listing (root-dir uri relative-path absolute-dir files)
  "Render the html of the directory"
  (let* ((root-ns (namestring root-dir))
         (root-len (length root-ns))
         (html-title (format nil "Index of /~A" (handler-case (url-decode relative-path)
                                                  (error () relative-path))))
         (up-pathname (if (pathname-equal absolute-dir root-dir)
                          nil
                          (pathname-parent-directory absolute-dir)))
         (up-link (if up-pathname ;
                      (let ((up-ns (namestring up-pathname)))
                        (cond
                          ((pathname-equal up-pathname root-dir)
                           (format nil "~A/" uri))
                          ((< root-len (length up-ns))
                           (format nil "~A/~A/" uri (encode-path-for-url (subseq up-ns root-len))))
                          (t (warn "Unexpected up-pathname bounds for ~A" up-ns)
                             (format nil "~A/" uri))))
                      nil)))
    (with-output-to-string (s)
      (format s "<html><head><title>~A</title></head><body>" html-title)
      (format s "<h1>Index of /~A</h1><hr><ul>" relative-path)
      (when up-link
        (format s "<li><a href=\"~A\">.. (Parent Directory)</a></li>" up-link))
      (dolist (file files)
        (let* ((filename (file-namestring file))
               (is-dir (directory-pathname-p file))
               (display-name (if is-dir
                                 (format nil "~A/" (directory-namestring% file))
                                 filename))
               (file-ns (namestring file))
               (file-len (length file-ns))
               (relative-path-segment
                 (cond
                   ((pathname-equal file root-dir) "")
                   ((< root-len file-len)
                    (subseq file-ns root-len))
                   (t
                    (warn "SUBSEQ boundary error avoided for file: ~A (Root Len: ~A, File Len: ~A)" file-ns root-len file-len)
                    "")))
               (has-trailing-slash (and (> (length relative-path-segment) 0)
                                        (char= (char relative-path-segment (1- (length relative-path-segment))) #\/)))
               (encoded-segment (encode-path-for-url (if has-trailing-slash
                                                         (subseq relative-path-segment 0 (1- (length relative-path-segment)))
                                                         relative-path-segment)))
               (url-path (format nil "~A/~A~A" uri encoded-segment (if has-trailing-slash "/" "")))
               (modified-time (if (probe-file file)
                                  (format-timestring nil (universal-to-timestamp (file-write-date file)))
                                  "N/A")))
          (format s "<li><a href=\"~A\">~A</a> (~A, ~A)</li>"
                  url-path
                  display-name
                  (if is-dir "DIR" (format nil "~D bytes" (file-size-in-octets file)))
                  modified-time)))
      (format s "</ul><hr></body></html>"))))

(defun directory-listing-handler (root-dir uri params)
  "Handler for the requests to list the directory."
  (let* ((relative-path-str (or (car (cdr (assoc :splat params))) ""))
         (relative-path-decoded (handler-case (url-decode relative-path-str)
                                  (error () relative-path-str)))
         (normalized-root-dir (pathname-as-directory root-dir))
         (relative-path-pn (if (string= "" relative-path-decoded)
                               nil
                               (pathname-as-directory relative-path-decoded)))
         (absolute-path (if relative-path-pn
                            (merge-pathnames-as-directory normalized-root-dir relative-path-pn)
                            normalized-root-dir)))
    (cond
      ((not (directory-exists-p absolute-path))
       (setf (lack.response:response-status ningle:*response*) 404)
       "404 Not Found")
      (t
       (let ((files (directory-files absolute-path)))
         (list 200
               '(:content-type "text/html; charset=utf-8")
               (list (render-directory-listing
                      root-dir
                      uri
                      relative-path-decoded
                      absolute-path
                      files))))))))

(defun file-download-handler (root-dir uri params)
  "Handler for the requests to download the file."
  (let* ((relative-path-raw (car (cdr (assoc :splat params))))
         (relative-path (handler-case (url-decode relative-path-raw)
                          (error () relative-path-raw)))
         (absolute-path (merge-pathnames relative-path root-dir)))
    (cond
      ((not (probe-file absolute-path))
       (setf (lack.response:response-status ningle:*response*) 404)
       "404 Not Found")
      ((directory-pathname-p absolute-path)
       (setf (lack.response:response-status ningle:*response*) 302
             (lack.response:response-headers ningle:*response*)
             `(:location ,(format nil "~A/~A/" uri (encode-path-for-url relative-path))))
       nil)
      (t
       (let* ((mime-type (mime-type-from-pathname absolute-path))
              (file-size (file-size-in-octets absolute-path))
              (fname (file-namestring absolute-path))
              (fname-enc (url-encode fname))
              (fname-ascii fname-enc))
         (list 200
               `(:content-type ,mime-type
                 :content-length ,file-size
                 :content-disposition ,(format nil "attachment; filename=\"~A\"; filename*=UTF-8''~A" fname-ascii fname-enc))
               absolute-path))))))

(defvar *401-content* "401 Unauthorized")

(defgeneric verify-auth (params)
  (:documentation "Verify authentication."))

(defmethod verify-auth (params)
  "All pass for this method. You should specialize you own method."
  (declare (ignore params))
  t)

(defun serve-directory (app serve-dir uri)
  "uri: string, should begin with a slash and cannot end with slash."
  (let ((root-dir (truename serve-dir))) ; expand tilde path such as "~/bin"
    ;; route1: listing directories (match all paths end with '/', or empty paths), eg. /files/, /files/sub/
    (setf (ningle:route app (format nil "~d/*/" uri))
          #'(lambda (params)
              (let ((passedp (verify-auth params)))
                (if passedp
                    (funcall #'directory-listing-handler root-dir uri params)
                    (progn
                      (setf (lack.response:response-status ningle:*response*) 401)
                      *401-content*)))))
    ;; route 2: download a file (the paths not end with '/'), eg. /files/file.txt, /files/sub/file.txt
    (setf (ningle:route app (format nil "~d/*" uri))
          #'(lambda (params)
              (let ((passedp (verify-auth params)))
                (if passedp
                    (funcall #'file-download-handler root-dir uri params)
                    (progn
                      (setf (lack.response:response-status ningle:*response*) 401)
                      *401-content*)))))
    ;; route 3: the root of the directory
    (setf (ningle:route app (format nil "~d" uri))
          (lambda (params)
            (let ((passedp (verify-auth params)))
              (if passedp
                  (progn
                    (setf (lack.response:response-status ningle:*response*) 302
                          (lack.response:response-headers ningle:*response*) `(:location ,(format nil "~d/" uri)))
                    nil)
                  (progn
                    (setf (lack.response:response-status ningle:*response*) 401)
                    *401-content*)))))
    (values app)))
