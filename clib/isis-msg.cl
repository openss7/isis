;;;  $RCSfile: isis-msg.cl,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:10 $  
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
          make-c-short-vector c-short-vector-ref
          make-c-byte-vector c-byte-vector-ref
          make-c-ptr-vector c-ptr-vector-ref c-ptr-vector-to-list
          make-c-address-vector c-address-vector-ref c-address-vector-set
          c-address-vector-to-address-list address-list-to-c-address-vector
          c-string-to-lisp-string

          long long-vector make-long-vector make-long long-value
          short short-vector make-short-vector make-short short-value
          byte-8 byte-vector make-byte-vector make-byte byte-value

          byte char address message bitvec event
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

;;; USE/IMPORTS
(require 'foreign)
(use-package :foreign-functions)

;;; Creation and usage of C values from lisp.
;;; (None of this is explained very clearly in the Allegro manual.)
;;; 
;;; There are two ways to do this:
;;; (1) Create C objects in C space using malloc. This is the safest method.
;;; The object lives in C space and is never looked at or relocated by the
;;; lisp garbage collector (GC). Objects must be explicitly freed. Allegro
;;; provides some support for this usage with the cstructs facility which
;;; allows definition of a C struct and allocation by malloc.
;;;
;;; (2) Create lisp objects and pass references to them to C. C can 
;;; understand these objects by using the include file lib/misc/lisp.h
;;; and for simple arrays of 32 bit, 16 bit and 8 bits values the actual
;;; representation of array elements is identical to C's. (Lisp's
;;; foreign-function interface automatically coerces a simple array
;;; argument into the kind of pointer C expects. 
;;; All such lisp objects passed to C may move, or be deleted 
;;; upon a GC. GCs are disabled when executing in C code, as 
;;; is process preemption (otherwise a GC could happen at
;;; any time in C code when a timeslice occurred). So a GC can occur in C
;;; code only if an explicit call back into lisp is made, or an explicit
;;; process switch (e.g. an ISIS t_wait). Neither of these occurs with
;;; the msg-get and msg-put routines, so they are safe with this technique.
;;; If GCs may occur, you must use the register-value mechanism to register
;;; lisp objects before passing them to C, and C code must re-get the 
;;; lisp references any time a GC could have happened. 
;;;
;;; With bcast, a process switch may occur when we wait for reply messages,
;;; so we use method (1) to pass values in the bcast call. (We could have
;;; used method (2) and sprinkled lisp_value calls at appropriate places
;;; in the C code.)

;;; Address structure: malloced in C space.
;;; Must be the same as the definition in msg.h.
;;; (Could have two defcstructs: one malloced and the other GCed)
;;; We never use the make-address function provided by this defcstruct.
;;; Instead we use (address) and (save-address) which calls into C and 
;;; stores addresses in a canonicalized table to save storage.
;;; Thus addresses should never be freed.

(defcstruct (address :malloc)
  (process-or-group-id :short)
  (site :unsigned-byte)
  (incarn :unsigned-byte)
  (port-no :short)
  (entry :short))
(defconstant *sizeof-address* 12)

(defun address-process (addr)        ; Really should check address-type.
  (address-process-or-group-id addr))
(defun address-group-id (addr)
  (address-process-or-group-id addr))

;;;
;;; Malloc and freeing of scalar c "variables" from lisp.
;;;

(defcstruct (c-long :malloc) (value :long))
   ; Defines make-c-long which mallocs a one-word container for a long,
   ; and c-long-value which extracts the long value out of such a container.
(defcstruct (c-short :malloc) (value :short))
(defcstruct (c-byte :malloc) (value :byte))

;;; Just use (free x) or (free-cstruct x) to free these values.

;;; Malloced C vectors. 
;;; We define a dummy c-struct (containing only one element vector)
;;; which nevertheless provides a correct implementation of array access.
;;; We then allocate varying length vectors by calling malloc directly.

(defcstruct (c-long-vec :malloc) (ref 1 :long))
(defun make-c-long-vector (n)
  (malloc (* n 4)))
(defmacro c-long-vector-ref (v n) `(c-long-vec-ref ,v ,n))
   ; This works with setf.
(defun c-long-vector-to-list (v n)
  (do ((l nil)
       (i (- n 1) (- i 1)))
      ((minusp i) l)
    (setq l (cons (c-long-vector-ref v i) l))))

(defcstruct (c-short-vec :malloc) (ref 1 :short))
(defun make-c-short-vector (n)
  (malloc (* n 2)))
(defmacro c-short-vector-ref (v n) `(c-short-vec-ref ,v ,n))

(defcstruct (c-byte-vec :malloc) (ref 1 :byte))
(defun make-c-byte-vector (n)
  (malloc n))
(defmacro c-byte-vector-ref (v n) `(c-byte-vec-ref ,v ,n))

(defcstruct (c-ptr-vec :malloc) (ref 1 :long))
(defun make-c-ptr-vector (n)
  (malloc (* n 4)))
(defmacro c-ptr-vector-ref (v n) `(c-ptr-vec-ref ,v ,n))
(defun c-ptr-vector-to-list (v n)
  (do ((l nil)
       (i (- n 1) (- i 1)))
      ((minusp i) l)
    (setq l (cons (c-ptr-vector-ref v i) l))))

(defcstruct (c-address-vec :malloc) (ref 1 address))
(defun make-c-address-vector (n)
  (malloc (* n *sizeof-address*)))
(defun c-address-vector-ref (v n) (+ v (* n *sizeof-address*)))
  ;; Will not work with setf
(defun c-address-vector-set (v n a)
  (bcopy a (+ v (* n *sizeof-address*)) *sizeof-address*))

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

(defvar *max-c-string-len* 100000)
             ;; C strings longer than this are assumed to be errors

(defun c-string-to-lisp-string (char-ptr)
  "Returns a lisp string whose contents is a copy of the null-terminated
   string pointed to by char-ptr"
  (declare (integer char-ptr))
  (let ((n (strlen char-ptr)))
    (if (> n *max-c-string-len*) ; Reasonableness check
        (cerror "Assume C string is OK"
                "Trying to convert very long (~D) C string" n))
    (let ((s (make-string n) ))
      (strncpy s char-ptr n)
      s)))

;;; Lisp simple arrays which C understands:
;;; These have element types of 32, 16 or 8 bit values, and are
;;; layed out in the way C expects.
;;; Do not confuse these with lisp's fixnums (29 bits long). C
;;; does not understand arrays of fixnums.

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
;;;        form (make-var) . If make-var does a malloc, then that 
;;;        object is freed. In any case the value from body is
;;;        returned as the result of the with-c-var form.
;;; (with-c-var (x) body)
;;;     -- is short for (with-c-var (x (make-long)) body)
;;;

(defmacro with-c-var ((x (&optional (make-var 'make-long))) &body body)
  (if (mismatch (symbol-name make-var) "MAKE-C-" :end1 7)
      `(let ((,x (,make-var)))  ; Lisp object
           ,@body)
      `(progn                   ; Malloced object
        (let ((,x (,make-var)))
          (prog1
              (progn ,@body)
            (free ,x))))))

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
;;; Since none of the msg-get/put routines block or call back into
;;; user code we can pass lisp objects to C. (Currently isis_define_type
;;; is not callable from lisp. If it were then message routine could call 
;;; back into lisp!)
;;;
;;; Possible improvements to this code:
;;; - Remove msg-put-xxx functions and add setf macro forms for msg-xxx
;;; - Call msg_gettype and msg_getfield directly (would need to bump
;;;   SYSFLD_SCAN current pointer though) to avoid garbage resulting
;;;   from with-c-var calls.
;;; - OR obviate garbage by having per-process globals.

(defun msg-long (m)
  (with-c-var (x)
    (msg-get m "%d" x)
    (long-value x)))
(defun msg-put-long (m n)
  (msg-put m "%d" n)
  m)

(defun msg-short (m)
  (with-c-var (x (make-short))
    (msg-get m "%h" x)
    (short-value x)))
(defun msg-put-short (m n)
  (msg-put m "%h" n)
  m)

(defun msg-byte (m)
  (with-c-var (x (make-byte))
    (msg-get m "%c" x)
    (byte-value x)))
(defun msg-put-byte (m n)
  (msg-put m "%c" n)
  m)

(defun msg-char (m)
  (code-char (msg-byte m)))
(defun msg-put-char (m c)
  (msg-put-byte m (char-code c))
  m)

(defun msg-string (m)
  (with-c-var (x)
    (msg-get m "%-s" x)
    (c-string-to-lisp-string (long-value x))))
(defun msg-put-string (m s)
  (msg-put m "%s" s)
  m)

(defun msg-message (m)
  (with-c-var (x)
    (msg-get m "%m" x)
    (long-value x)))
(defun msg-put-message (m m1)
  (msg-put m "%m" m1)
  m)

(defun msg-address (m)
  (with-c-var (x)
    (msg-get m "%+A[1]" x)
    (long-value x)))
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
;;; Array/vector message operations. Again these pass lisp objects to C.
;;;

(defun msg-long-vector (m)
  (with-c-var (x)                   ; Vector pointer var
    (with-c-var (z)                 ; Vector size var
      (msg-get m "%-D" x z)
      (let* ((n (long-value z))   ; Vector size in longs
             (v (make-long-vector n)))
        (bcopy (long-value x) v (* n 4))
        v))))
(defun msg-put-long-vector (m v)
  (declare (type (long-vector *) v))
  (msg-put m "%D" v (length v))
  m)

(defun msg-short-vector (m)
  (with-c-var (x)                   ; Vector pointer var
    (with-c-var (z)                 ; Vector size var
      (msg-get m "%-H" x z)
      (let* ((n (long-value z))   ; Vector size in shorts
             (v (make-short-vector n)))
        (bcopy (long-value x) v (* n 2))
        v))))
(defun msg-put-short-vector (m v)
  (declare (type (short-vector *) v))
  (msg-put m "%H" v (length v))
  m)

(defun msg-byte-vector (m)
  (with-c-var (x)                   ; Vector pointer var
    (with-c-var (z)                 ; Vector size var
      (msg-get m "%-C" x z)
      (let* ((n (long-value z))   ; Vector size in bytes
             (v (make-byte-vector n)))
        (bcopy (long-value x) v n)
        v))))
(defun msg-put-byte-vector (m v)
  (declare (type (byte-vector *) v))
  (msg-put m "%C" v (length v))
  m)

(defun msg-address-list (m)
  (with-c-var (x)                   ; Vector pointer var
    (with-c-var (z)                 ; Vector size var
      (msg-get m "%-A" x z)
      (c-address-vector-to-address-list (long-value x) (long-value z)))))
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
  (c-address-vector-to-address-list (msg-getdests-c msg)))

;;;
;;; Broadcast interface.
;;; Since bcast may block, we only pass malloced objects.
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

(defun bc-address (dest)
  ;; Return a list containing the address argument(s) for bcast_l.
  ;; dest is a list in one of the following forms:
  ;; ( addr [ entry ] )      -- single address and optional entry number
  ;; ( ( addr+ ) [ entry ] ) -- address list and optional entry number
  ;;
  ;; The result is either a list of length 1 containing a pointer
  ;; to an address vector, or a list of length 2 containing a pointer
  ;; to an address followed by the entry number.

  (let ((addr (first dest))    ; Either an address or a list of addresses
        (entry (the (or null fixnum) (second dest))))
                               ; Nil if no entry number supplied
    (typecase addr
      ;; Most common case first: single address with optional entry number.
      (integer (list addr (if (null entry)
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
                       (setf (c-address-vec-ref-entry av i) entry))
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
            (let* ((mvec (make-c-long-vector n-wanted))
                   (result (apply bcast-func
                                  (append bc-args (list msg n-wanted mvec)))))
              (prog1
                  (if (plusp result)
                      (c-long-vector-to-list mvec result) ; Replies
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
  ;; Compute the reply-l options string.
  (let ((options (make-array 3 :element-type 'string-char
                               :initial-contents "m  "
                               :fill-pointer 1)))
    (if exclude
        (vector-push #\x options))
    (if carbon-copies
        (vector-push #\c options))
    (coerce options 'simple-string)))

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

(defun groupview-name (gv)
  (c-string-to-lisp-string (get-gv-name gv)))
(defun groupview-members (gv)
  (c-address-vector-to-address-list (get-gv-members gv) (get-gv-nmemb gv)))
(defun groupview-clients (gv)
  (c-address-vector-to-address-list (get-gv-clients gv) (get-gv-nclient gv)))

;;; To do:
;;; Various msg-long, msg-byte-vector functions need to check for 
;;; format mismatches and report an error. Currently they use the junked
;;; values returned from msg-get and proceed to get blow up later, confusing 
;;; the programmer.
