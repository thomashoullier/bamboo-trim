(defsystem bamboo-trim
  :name "bamboo-trim"
  :author "Thomas HOULLIER"
  :depends-on ("alexandria")
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "bamboo" :depends-on ("package"))
                 (:file "cutter" :depends-on ("bamboo"))
                 (:file "sim" :depends-on ("bamboo" "cutter"))
                 (:file "sim-to-file" :depends-on ("sim"))))))
