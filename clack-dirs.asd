(asdf:defsystem #:clack-dirs
  :description "Serving directories and files for Clack and Ningle."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :version "1.0.1"
  :serial t
  :depends-on (#:str
               #:cl-fad
               #:local-time
               #:trivial-mimes
               #:trivial-file-size
               #:quri
               #:lack
               #:clack
               #:ningle)
  :components ((:file "main")))
