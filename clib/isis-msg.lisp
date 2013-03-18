;;;  $RCSfile: isis-msg.lisp,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:12 $  
;;; -*- Mode:Lisp; Package:ISIS; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-
;;; Coded by Robert Cooper
;;; 
;;;
;;;
;;;      ISIS release V2.0, May 1990
;;;      Export restrictions apply
;;;
;;;      The contents of this file are subject to a joint, non-exclusive
;;;      copyright by members of the ISIS Project.  Permission is granted for
;;;      use of this material in unmodified form in commercial or research
;;;      settings.  Creation of derivative forms of this software may be
;;;      subject to restriction; obtain written permission from the ISIS Project
;;;      in the event of questions or for special situations.
;;;      -- Copyright (c) 1990, The ISIS PROJECT
;;;
;;;
;;; allegro-CL ISIS interface: message support.
;;;

(in-package "ISIS")         ;; Only part of the isis package actually.
(provide 'isis-msg)

;;; EXPORTS
(export '(address-process
          address-groupid
          address-port-no
          address-cluster
          address-site
          address-incarn
          address-entry
          address-type

          message msg-put-any
          message-list msg-any
          msg-long msg-put-long
          msg-short msg-put-short
          msg-byte msg-put-byte
          msg-char msg-put-char
          msg-string msg-put-string
          msg-address msg-put-address
          msg-message msg-put-message
          msg-bitvec msg-put-bitvec
          msg-event msg-put-event
          msg-groupview msg-put-groupview

          make-c-long-vector c-long-vector-ref c-long-vector-to-list
          make-c-short-vector c-short-vector-ref c-short-vector-to-list
          make-c-byte-vector c-byte-vector-ref c-byte-vector-to-list
          make-c-ptr-vector c-ptr-vector-ref c-ptr-vector-to-list
          make-c-address-vector c-address-vector-ref c-address-vector-set
          c-address-vector-to-address-list address-list-to-c-address-vector
          c-string-to-lisp-string

          long long-vector make-long-vector make-long long-value
          short short-vector make-short-vector make-short short-value
          byte-8 byte-vector make-byte-vector make-byte byte-value

          void byte char address message bitvec event
          groupview address-list

          msg-long-vector msg-put-long-vector
          msg-short-vector msg-put-short-vector
          msg-byte-vector msg-put-byte-vector
          msg-address-list msg-put-address-list
          msg-getdests

          bcast abcast cbcast gbcast fbcast all majority
          reply

          groupview-name
          groupview-members 
          groupview-clients 
         ))

;;; Creation and usage of C values from Lucid.
;;; 
;;; There are two ways to do this:
;;;
;;; (1) Create C objects in C space using malloc. This is the best method.
;;; The object lives in C space and is never looked at or relocated by the
;;; lisp garbage collector (GC). Objects must be explicitly freed. Allegro
;;; provides some support for this usage with the cstructs facility which
;;; allows definition of a C struct and allocation by malloc.
;;;
;;; (2) Addresses of stationary objects can be passed freely to, and 
;;; manipulated by C code. But stationary objects must be interned
;;; values in a package, constants, or objects created in a 
;;; with-static-area macro. The latter are never garbaged collected and
;;; it appears they can't be deleted explicitly either. This is therefore
;;; not a sufficiently general mechanism to use.

;;; Address structure: malloced in C space.
;;; Must be the same as the definition in msg.h.
;;; We never use the make-address function provided by this defcstruct.
;;; Instead we use (address) and (save-address) which calls into C and 
;;; stores addresses in a canonicalized table to save storage.
;;; Thus addresses should never be freed.

(def-foreign-struct address
  (process :type :signed-16bit)
  (group-id :type :signed-16bit :overlays process)
  (site :type :unsigned-8bit)
  (incarn :type :unsigned-8bit)
  (port-no :type :signed-16bit)
  (entry :type :unsigned-16bit))

;;;
;;; Malloc and freeing of scalar c "variables" from lisp.
;;;

(def-foreign-struct c-long (value :type :signed-32bit))
   ; Defines make-c-long which mallocs a one-word container for a long,
   ; and c-long-value which extracts the long value out of such a container.
(def-foreign-struct c-char-ptr (value :type (:pointer :character)))
(def-foreign-struct c-short (value :type :signed-16bit))
(def-foreign-struct c-byte (value :type :signed-8bit))

;;; Just use (free x) or (free-foreign-pointer x) to free these values.

;;; Malloced C vectors. 
;;; We define a dummy c-struct (containing only one element vector)
;;; which nevertheless provides a correct implementation of array access.
;;; We then allocate varying length vectors by calling malloc directly.

(defun make-c-long-vector (n)
  (malloc-foreign-pointer :type `(:pointer (:array :signed-32bit (,(* n 4))))))
(defmacro c-long-vector-ref (v n)
  `(foreign-aref ,v ,n))
   ; This works with setf.
(defun c-long-vector-to-list (v n)
  (do ((l nil)
       (i (- n 1) (- i 1)))
      ((minusp i) l)
    (setq l (cons (c-long-vector-ref v i) l))))


(defun make-c-short-vector (n)
  (malloc-foreign-pointer :type `(:pointer (:array :signed-16bit (,n)))))
(defmacro c-short-vector-ref (v n)
  `(foreign-aref ,v ,n))

(defun make-c-byte-vector (n)
  (malloc-foreign-pointer :type `(:pointer (:array :signed-8bit (,n)))))
(defmacro c-byte-vector-ref (v n)
  `(foreign-aref ,v ,n))

(defun make-c-ptr-vector (n)
  (malloc-foreign-pointer :type `(:pointer
                                  (:array (:pointer void) (,(* n 4))))))
(defmacro c-ptr-vector-ref (v n)
  `(foreign-aref ,v ,n))
(defun c-ptr-vector-to-list (v n)
  (do ((l nil)
       (i (- n 1) (- i 1)))
      ((minusp i) l)
    (setq l (cons (c-ptr-vector-ref v i) l))))

(defun make-c-address-vector (n)
  (malloc-foreign-pointer :type `(:pointer (:array address (,n)))))
(defmacro c-address-vector-ref (v n)
  `(foreign-aref ,v ,n))

(defun c-address-vector-set (v n a)
  (setf (foreign-aref v n) a))

(defun c-address-vector-to-address-list (v &optional n)
  "Makes a list of the addresses in address vector v.
   If the length n is omitted, v is assumed to be a null-terminated 
   address vector"
  (let ((l nil))
    (if (null n)
        (do* ((i 0 (+ i 1))
              (addr (c-address-vector-ref v i)))
             ((addr-isnull addr) (reverse l))
          (setq l (cons (save-address addr) l)))
        (do ((i (- n 1) (- i 1)))
            ((minusp i) l)
          (setq l (cons (save-address (c-address-vector-ref v i)) l))))))

(defun address-list-to-c-address-vector (addr-list null-term)
  ;; Make a malloced address vector from addr-list
  ;; Make it null terminated if null-term is non-nil
  (let* ((addr-len (length addr-list))
         (v (make-c-address-vector (if null-term
                                      (+ 1 addr-len)
                                      addr-len))))
    (do ((i 0 (+ i 1))
         (l addr-list (rest l)))
        ((null l))
      (c-address-vector-set v i (first l)))
    (if null-term
        (c-address-vector-set v addr-len (nulladdress)))
    v))

;;;
;;; Convert C strings into lisp strings.
;;;

(defun c-string-to-lisp-string (char-ptr)
  "Returns a lisp string whose contents is a copy of the null-terminated
   string pointed to by char-ptr"
  (foreign-string-value char-ptr))

;;; Lisp simple arrays into which we copy the contents of C malloced arrays:

(deftype long () '(signed-byte 32))
(deftype long-vector (&optional n) `(simple-array long (,n)))
(defun make-long-vector (n) (make-array n :element-type 'long))
(defun make-long () (make-long-vector 1))
(defun long-value (v) (aref v 0))

(deftype short () '(signed-byte 16))
(deftype short-vector (&optional n) `(simple-array short (,n)))
(defun make-short-vector (n) (make-array n :element-type 'short))
(defun make-short () (make-short-vector 1))
(defun short-value (v) (aref v 0))

(deftype byte-8 () '(signed-byte 8))
(deftype byte-vector (&optional n) `(simple-array byte-8 (,n)))
(defun make-byte-vector (n) (make-array n :element-type 'byte-8))
(defun make-byte () (make-byte-vector 1))
(defun byte-value (v) (aref v 0))

;;; with-c-var is a form which can be called in two ways:
;;; (with-c-car (x (make-var) body))    
;;;     -- executes body with x bound to the object returned by the 
;;;        form (make-var), which mallocs a "variable" in C space.
;;;        After the body this object is freed. The value from the body is
;;;        returned as the result of the with-c-var form.
;;; (with-c-var (x) body)
;;;     -- is short for (with-c-var (x (make-c-long)) body)
;;;

(defmacro with-c-var ((x &optional (make-var '(make-c-long))) &body body)
      `(progn                   
        (let ((,x ,make-var))
          (prog1
              (progn ,@body)
            (free ,x)))))

;;;
;;; Generic msg-put/get functions.
;;;

;;; Constants must be the same as in msg.h
(defconstant ftype-address (char-code #\a))
(defconstant ftype-bitvec (char-code #\b))
(defconstant ftype-char (char-code #\c))
(defconstant ftype-short (char-code #\h))
(defconstant ftype-long (char-code #\d))
(defconstant ftype-event (char-code #\e))
(defconstant ftype-float (char-code #\f))
(defconstant ftype-double (char-code #\g))
(defconstant ftype-pgroup (char-code #\p))
(defconstant ftype-message (char-code #\m))
(defconstant ftype-siteid #X80)
(defconstant ftype-groupview #X82)
(defconstant ftype-deleted #XFF)

(defmacro message (&rest arg-list)
  (cons 'message-put
        (mapcar '(lambda (arg) (if (listp arg)
                                   (cons 'list arg)
                                   arg))
                        arg-list)))

(defun message-put (&rest arg-list)
  (let ((m (msg-newmsg)))
    (do* ((args arg-list (rest args)))
         ((null args))
      (let ((arg (first args)))
        (if (listp arg)
            (msg-put-any m (first arg) (second arg))
            (msg-put-any m arg))))
    m))

(defun message-list (m &rest type-list-or-number)
  (let ((result nil))
    (if (and (= (length type-list-or-number) 1)
             (numberp (first type-list-or-number)))

        ;; Number of results desired
        (let ((n (first type-list-or-number)))
          (dotimes (i n)
            (setq result (cons (msg-any m) result))))

        (if (plusp (length type-list-or-number))
            ;; List of types
            (do* ((types type-list-or-number (rest types)))
                 ((null types))
              (setq result (cons (msg-any m (car types)) result)))
            
            ;; Bad arguments.
            (error "must specify number of message fields or their types")))
    (reverse result)))

(defun msg-any (m &optional type)
  "Extract the next field from m. If type is specified the field must
   be compatible with that type. If type is omitted a type based on
   the message field is used"
  ;; Format letters (used in C code)   default Lisp type
  ;;           "c" or "s"                  string
  ;;           "h"                         short
  ;;           "l" "d" "D" "L"             long
  ;;           "a" "A"                     address
  ;;           "m"                         message
  ;;           "b" "B"                     bitvec
  ;;           "p" "P"                     groupview
  ;;           "e" "E"                     event
  ;; It may be better to see if the number of elements is 1 or more than
  ;; one and return a scalar or vector accordingly.

  (if (null type)
      (setq type
            (let ((ftype (msg-getscantype m)))
              ;; Unfortunately its not straightforward to find out
              ;; if a field is a vector of a scalar value. (In particular
              ;; a vector of size 1 and a scalar are indistinguishable)
              ;; So we have some defaults which the user can override.

              (cond 
                ((eql ftype ftype-char) 'string) ; Assume its a string.

                ;; For the rest assume its scalar.
                ((eql ftype ftype-short) 'short)
                ((eql ftype ftype-long) 'long)
                                        ; A cursory look at msg_dogetf
                                        ; suggests that specifying %d instead
                                        ; of %D causes disaster however!
                ((eql ftype ftype-address) 'address) 
                ((eql ftype ftype-message) 'message) 
                ((eql ftype ftype-bitvec)  'bitvec)   
                ((eql ftype ftype-groupview) 'groupview) 
                ((eql ftype ftype-event)   'event)
                (t (error "unknown field type ~s in message" ftype))))))
  (case type
    ('long       (msg-long m))
    ('short      (msg-short m))
    ('byte       (msg-byte m))
    ('char       (msg-char m))
    ('string     (msg-string m))
    ('address    (msg-address m))
    ('message    (msg-message m))
    ('bitvec     (msg-bitvec m))
    ('event      (msg-event m))
    ('groupview  (msg-groupview m))
    ('long-vector  (msg-long-vector m))
    ('short-vector (msg-short-vector m))
    ('byte-vector  (msg-byte-vector m))
    ('address-list (msg-address-list m))
    (t (error "unknown field type ~s" type))
    ))
  
(defun msg-put-any (m val &optional type)
  "Insert val into message m. If type is not specified try to guess
   the appropriate type (this is not very sound). If type is specified
   this indicates which field type to use"
  (if (null type)
      (setq type
            (typecase val
              (fixnum       'long)
              (long         'long)
              (short        'short)
              (byte-8       'byte)
              (string       'string)
              (character    'char)
              ((list integer) 'address-list) ; Kind of risky assumption
              (long-vector  'long-vector)
              (short-vector 'short-vector)
              (byte-vector  'byte-vector)
              (t (error "cant' choose default field type for ~s of type ~s"
                        val (type-of val))))))
  (case type
    ('long       (msg-put-long m val))
    ('short      (msg-put-short m val))
    ('byte       (msg-put-byte m val))
    ('char       (msg-put-char m val))
    ('string     (msg-put-string m val))
    ('address    (msg-put-address m val))
    ('message    (msg-put-message m val))
    ('bitvec     (msg-put-bitvec m val))
    ('event      (msg-put-event m val))
    ('groupview  (msg-put-groupview m val))
    ('long-vector  (msg-put-long-vector m val))
    ('short-vector (msg-put-short-vector m val))
    ('byte-vector  (msg-put-byte-vector m val))
    ('address-list (msg-put-address-list m val))
    (t (error "unknown field type ~s" type))
    ))

;;; Functions for getting/putting individual values from/into messages.
;;; Possible improvements to this code:
;;; - Remove msg-put-xxx functions and add setf macro forms for msg-xxx
;;; - Call msg_gettype and msg_getfield directly (would need to bump
;;;   SYSFLD_SCAN current pointer though) to avoid garbage resulting
;;;   from with-c-var calls.
;;; - OR obviate garbage by having per-process globals.

(defun msg-long (m)
  (with-c-var (x)
    (msg-get m "%d" x)
    (c-long-value x)))
(defun msg-put-long (m n)
  (msg-put m "%d" n)
  m)

(defun msg-short (m)
  (with-c-var (x (make-c-short))
    (msg-get m "%h" x)
    (c-short-value x)))
(defun msg-put-short (m n)
  (msg-put m "%h" n)
  m)

(defun msg-byte (m)
  (with-c-var (x (make-c-byte))
    (msg-get m "%c" x)
    (c-byte-value x)))
(defun msg-put-byte (m n)
  (msg-put m "%c" n)
  m)

(defun msg-char (m)
  (code-char (msg-byte m)))
(defun msg-put-char (m c)
  (msg-put-byte m (char-code c))
  m)

(defun msg-string (m)
  (with-c-var (x (make-c-char-ptr))
    (msg-get m "%-s" x)
    (c-string-to-lisp-string (c-char-ptr-value x))))
(defun msg-put-string (m s)
  (msg-put m "%s" s)
  m)

(defun msg-message (m)
  (with-c-var (x)
    (msg-get m "%m" x)
    (c-long-value x)))
(defun msg-put-message (m m1)
  (msg-put m "%m" m1)
  m)

(defun msg-address (m)
  (with-c-var (x)
    (msg-get m "%+A[1]" x)
    (c-long-value x)))
(defun msg-put-address (m a)
  (msg-put m "%A[1]" a)
  m)

;;; The rest use exactly the same method as addresses.
(defun msg-bitvec (m) (msg-address m))
(defun msg-event (m) (msg-address m))
(defun msg-groupview (m) (msg-address m))
(defun msg-put-bitvec (m b) (msg-put-address m b))
   ;; Could support proper lisp bitvecs sometime I guess.
(defun msg-put-event (m e) (msg-put-address m e))
(defun msg-put-groupview (m g) (msg-put-address m g))

;;;
;;; Array/vector message operations.
;;;

(defun msg-long-vector (m)
  (with-c-var (x)                   ; Vector pointer var
    (with-c-var (z)                 ; Vector size var
      (msg-get m "%-D" x z)
      (let* ((n (c-long-value z))   ; Vector size in longs
             (cv (make-foreign-pointer ; Pointer to C vector.
                  :address (c-long-value x)
                  :type `(:pointer (:array :signed-32bit (,n)))))
             (lv (make-long-vector n))) ; Lisp vector.
        (dotimes (i n)
          (setf (aref lv i) (foreign-aref cv i)))
        lv))))
(defun msg-put-long-vector (m lv)
  (declare (type (long-vector *) lv))
  (let* ((n (length lv))
         (cv (malloc-foreign-pointer
              :type `(:pointer (:array :signed-32bit (,n))))))
    (dotimes (i n)
      (setf (foreign-aref cv i) (aref lv i)))
    (msg-put m "%D" cv n))
  m)

(defun msg-short-vector (m)
  (with-c-var (x)                   ; Vector pointer var
    (with-c-var (z)                 ; Vector size var
      (msg-get m "%-H" x z)
      (let* ((n (c-long-value z))   ; Vector size in shorts
             (cv (make-foreign-pointer ; Pointer to C vector.
                  :address (c-long-value x)
                  :type `(:pointer (:array :signed-16bit (,n)))))
             (lv (make-short-vector n))) ; Lisp vector.
        (dotimes (i n)
          (setf (aref lv i) (foreign-aref cv i)))
        lv))))
(defun msg-put-short-vector (m lv)
  (declare (type (short-vector *) lv))
  (let* ((n (length lv))
         (cv (malloc-foreign-pointer
              :type `(:pointer (:array :signed-16bit (,n))))))
    (dotimes (i n)
      (setf (foreign-aref cv i) (aref lv i)))
    (msg-put m "%H" cv n))
  m)

(defun msg-byte-vector (m)
  (with-c-var (x)                   ; Vector pointer var
    (with-c-var (z)                 ; Vector size var
      (msg-get m "%-C" x z)
      (let* ((n (c-long-value z))   ; Vector size in bytes
             (cv (make-foreign-pointer ; Pointer to C vector.
                  :address (c-long-value x)
                  :type `(:pointer (:array :signed-8bit (,n)))))
             (lv (make-byte-vector n))) ; Lisp vector.
        (dotimes (i n)
          (setf (aref lv i) (foreign-aref cv i)))
        lv))))
(defun msg-put-byte-vector (m lv)
  (declare (type (byte-vector *) lv))
  (let* ((n (length lv))
         (cv (malloc-foreign-pointer
              :type `(:pointer (:array :signed-8bit (,n))))))
    (dotimes (i n)
      (setf (foreign-aref cv i) (aref lv i)))
    (msg-put m "%C" cv n))
  m)

(defun msg-address-list (m)
  (with-c-var (x)                   ; Vector pointer var
    (with-c-var (z)                 ; Vector size var
      (msg-get m "%-A" x z)
      (let ((n (c-long-value z)))
        (c-address-vector-to-address-list
         (make-foreign-pointer :address (c-long-value x)
                             :type `(:pointer (:array address (,n))))
         n)))))

(defun msg-put-address-list (m l)
  (let ((v (address-list-to-c-address-vector l nil)))
    (msg-put m "%A" v (length l))
    (free v))
  m)

;;; The rest aren't implemented yet: who needs them anyway?
;(defun msg-bitvec-vector (m)  (error "not implemented"))
;(defun msg-event-vector (m)  (error "not implemented"))
;(defun msg-groupview-vector (m)  (error "not implemented"))
;(defun msg-put-bitvec-vector (m b)  (error "not implemented"))
;(defun msg-put-event-vector (m e)  (error "not implemented"))
;(defun msg-put-groupview-vector (m g) (error "not implemented"))

(defun msg-getdests (msg)
  (c-address-vector-to-address-list
       (make-foreign-pointer :address (msg-getdests-c msg)
                             :type '(:pointer (:array address (1000000))))))
                               ;; Kluged type since foreign-aref can't 
                               ;; handle variable sized arrays!


;;;
;;; Broadcast interface.
;;;

(defun bc-options (exclude lazy addr-is-vector no-replies)
  ;; Compute the bcast_l options string.
  (let ((options (make-array 4 :element-type 'string-char
                               :fill-pointer 0)))
    (vector-push (if no-replies #\s #\m) options)
    (if exclude
        (vector-push #\x options))
    (if lazy
        (vector-push #\z options))
    (if addr-is-vector
        (vector-push #\l options))
    (coerce options 'simple-string)))


;;; Function bc-address.
;;; Return a list containing the address argument(s) for bcast_l.
;;; dest is a list in one of the following forms:
;;; ( addr [ entry ] )      -- single address and optional entry number
;;; ( ( addr+ ) [ entry ] ) -- address list and optional entry number
;;;
;;; The result is either a list of length 1 containing a pointer
;;; to an address vector, or a list of length 2 containing a pointer
;;; to an address followed by the entry number.

(defun bc-address (dest)
  (let ((addr (first dest))    ; Either an address or a list of addresses
        (entry (the (or null fixnum) (second dest))))
                               ; Nil if no entry number supplied
    (typecase addr
      ;; Most common case first: single address with optional entry number.
      (foreign-pointer
       (list addr (if (null entry)
                      (address-entry addr) ; Use entry from addr
                      entry)))
      (list    (if (null entry)
                   ;; Address list with no entry number.
                   (address-list-to-c-address-vector addr t)

                   ;; Address list with entry number.
                   (let ((av (address-list-to-c-address-vector addr t))
                         (alen (length addr)))
                     (do ((i 0 (+ i 1)))
                         ((= i alen))
                       (setf (address-entry (c-address-vector-ref av i))
                             entry))
                     av)))
      (t       (error "Bad destination for bcast: ~s" dest)))
    
    ))

(defconstant all-replies 10000)
(defconstant majority-replies 10001)

;;; bcast returns a list of reply messages (nil if no replies),
;;; or -1 if there was an error. (-1 is kind of non-lisp-ish!)

(defun generic-bcast (bcast-func dest msg 
                      &key ((:replies n-wanted) 0)
                           ((:exclude-sender exclude))
                           ((:lazy lazy)))
  (let* ((addr-args (bc-address dest))
         (addr-is-vector (= (length addr-args) 1))
         (no-replies (eql 0 n-wanted))
         (options (bc-options exclude lazy addr-is-vector no-replies))
         (bc-args (cons options addr-args)))

    (case n-wanted
      ('all (setq n-wanted all-replies))
      ('majority (setq n-wanted majority-replies)))
    (prog1
        (if no-replies

            ;; No reply case
            (if (zerop (apply bcast-func (append bc-args (list msg 0))))
                nil ; No errors, no replies.
                -1) ; Error.

            ;; Reply case
            (let* ((mvec (make-c-ptr-vector (max n-wanted *isis-max-procs*)))
                   (result (apply bcast-func
                                  (append bc-args (list msg n-wanted mvec)))))
              (prog1
                  (if (plusp result)
                      (c-ptr-vector-to-list mvec result)           ; Replies
                      (if (zerop result)
                          nil  ; No replies
                          -1)) ; Error
                (free mvec))))
      (if addr-is-vector
          (free (first addr-args)))
      )))

;;; Examples:
;;; (bcast (addr entry-nr) msg :replies 3)
;;; (bcast ((addr1 addr2 addr3) entry-nr) msg :replies 'all)
;;; (bcast ((addr1 addr2)) (msg-put-long (msg-newmsg) 123) 
;;;         :replies 'majority :lazy t)
;;; (bcast (addr) msg)

(defmacro bcast (dest &rest args)
  `(generic-bcast #'bcast-l (list ,@dest) ,@args))
(defmacro abcast (dest &rest args)
  `(generic-bcast #'abcast-l (list ,@dest) ,@args))
(defmacro cbcast (dest &rest args)
  `(generic-bcast #'cbcast-l (list ,@dest) ,@args))
(defmacro gbcast (dest &rest args)
  `(generic-bcast #'gbcast-l (list ,@dest) ,@args))
(defmacro fbcast (dest &rest args)
  `(generic-bcast #'fbcast-l (list ,@dest) ,@args))

(defun r-options (exclude carbon-copies)
  ;; Compute the reply_l options string.
  (let ((options (make-array 3 :element-type 'string-char
                               :initial-contents "m  "
                               :fill-pointer 1)))
    (if exclude
        (vector-push #\x options))
    (if carbon-copies
        (vector-push #\c options))
    (coerce options '(simple-array string-char))))

(defun reply (revd-msg reply-msg &key ((:cc addr-list))
                                      ((:exclude-sender exclude)))
  (let* ((carbon-copies (not (null addr-list)))
         (options (r-options exclude carbon-copies)))
    (if carbon-copies
        (let ((avec (address-list-to-c-address-vector addr-list t)))
          (prog1
              (reply-l options revd-msg avec reply-msg)
            (free avec)))
        (reply-l options revd-msg reply-msg))))

;;; Non-trivial groupview structure access functions.

(defun groupview-members (gv)
  (let ((n (get-gv-nmemb gv)))
    (c-address-vector-to-address-list
     (make-foreign-pointer :address (get-gv-members gv)
                           :type `(:pointer (:array address (,n))))
     n)))
(defun groupview-clients (gv)
  (let ((n (get-gv-nclient gv)))
    (c-address-vector-to-address-list 
     (make-foreign-pointer :address (get-gv-clients gv)
                           :type `(:pointer (:array address (,n))))
     n)))
   

