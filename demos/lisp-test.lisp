;;; ISIS Common lisp test program.
;;; Need to put the lisp directory in your load/require-path
;;; $RCSfile: lisp-test.lisp,v $ $Revision: 2.18 $ $Date: 90/08/27 13:36:55 $


(in-package "USER")
(require "isis")
(use-package "ISIS")

(isis-init 0)    ;; May need to set the ISIS port number here.

(defun-isis-entry handle-m1 1 (m)
   (let (n)                  
     (print "handle-1 called ")
     (prin1 (setq n (msg-any m)))
     (reply m (message ((* 2 n))))))

(defun-isis-entry handle-m2 2 (m)
   (let (n)                  
     (print "handle-2 called ")
     (prin1 (setq n (msg-any m)))
     (reply m (message ((* 2 n) 'short)))))

(defun char-invert-case (c)
  (if (both-case-p c)
      (if (upper-case-p c)
          (char-downcase c)
          (char-upcase c))
      c))

(defun string-invert-case (s)
  (map 'string #'char-invert-case s))

(defun-isis-entry handle-m3 3 (m)
   (let (s)                  
     (print "handle-3 called")
     (prin1 (setq s (msg-any m)))
     (reply m (message ((string-invert-case s))))))


(defun-isis-entry handle-m4 4 (m)
   (let (al)                  
     (print "handle-4 called")
     (setq al (msg-address-list m))
     (dolist (a al)
       (princ " ")
       (paddr a))
     (setq al (mapcar #'(lambda (a)
                          (address (address-site a)
                                   (address-incarn a)
                                   (address-process a)
                                   (+ 1 (address-entry a))))
                      al))
     (print "returning ")
     (dolist (a al)
       (princ " ")
       (paddr a))
     (reply m (message (al 'address-list)))))

(defun-isis-entry handle-m5 5 (m)
   (let (m1 bv)
     (print "handle-5 called")
     (setq m1 (msg-message m))
     (pmsg m1)

     (print "contents:")
     (setq bv (msg-byte-vector m1))
     (dotimes (i (length bv))
       (format t " ~s " (aref bv i)))

     (dotimes (i (- (length bv) 1))
       (let ((c (code-char (aref bv i))))
         (setf (aref bv i) (char-code (char-invert-case c)))))
     (reply m (message ((message (bv 'byte-vector)) 'message)))))

(defun msg-list-print (ml &optional printer)
  (if (null ml)
      (print "No replies")
      (dolist (m ml)
        (if printer
            (funcall printer m)
            (print (msg-any m)))))
  ml)

(defun-callback group-monitor (gview arg)
  (print "group-monitor")
  (print gview)
  (print arg)
  (format t "~%~a has ~d member~:p"
          (groupview-name gview) (length (groupview-members gview))))

(defun test ()
  (isis-start-done)
  (let ((gaddr (pg-join "mtest"
                        :monitor 'group-monitor)))
    (if (addr-isnull gaddr)
        (progn
          (isis-perror "joining mtest group")
          (error)))

    (print "Bcast m1")
    (msg-list-print (bcast (gaddr 1) (message 123) :replies 'all))

    (print "Bcast m1.1")
    (msg-list-print (bcast (gaddr 1) (message 212) :replies 'all))

    (print "Bcast m2")
    (msg-list-print (bcast (gaddr 2) (message (456 'short)) :replies 'all))

    (print "Bcast m3")
    (msg-list-print (bcast (gaddr 3) (message "abc") :replies 'all))

    (print "Bcast m4")
    (msg-list-print (bcast (gaddr 4)
                           (message ((my-address) 'address)) :replies 'all)
                    #'(lambda (m)
                        (terpri)
                        (dolist (a (msg-address-list m))
                          (paddr a))))

    (print "Bcast m4.1")
    (let* ((my (my-address))
           (al (list
                (address (address-site my)
                         (address-incarn my)
                         (address-process my)
                         1)
                (address (address-site my)
                         (address-incarn my)
                         (address-process my)
                         2)
                (address (address-site my)
                         (address-incarn my)
                         (address-process my)
                         3))))
      (msg-list-print (bcast (gaddr 4) (message (al 'address-list)) :replies 'all)
                    #'(lambda (m)
                        (terpri)
                        (dolist (a (msg-address-list m))
                          (paddr a)))))

    (print "Bcast m5")
    (msg-list-print
     (bcast (gaddr 5) (message ((message "zyxwvutsrq") 'message)) :replies 'all)
     #'(lambda (m)
         (terpri)
         (let* ((m1 (msg-message m))
                (bv (msg-byte-vector m1)))
           (dotimes (i (length bv))
             (format t " ~s " (aref bv i))))))
    "test completed"
    ))
