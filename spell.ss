
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2017                              *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

(define combine
  (lambda (x)
    (letrec
        ((process
         (lambda(listINPUT)
          (cond
            ((null? listINPUT) '())
            ((pair? listINPUT) (append(combine(car listINPUT)) (combine(cdr listINPUT))))
            (else(list listINPUT))
            )
           )
         ))
      (process x)
      )
    )
  )

(define genbitvector
  (lambda (x y)
    (letrec
        ((process
          (lambda(hashf ANS)
            (if(null? ANS)
             '()
             (cons (hashf (car ANS)) (genbitvector hashf(cdr ANS)))
             )
            )
          )
         )
      (process x y)
      )
    )
  )

(define combinevector
  (lambda (x y)
    (letrec
        ((process
         (lambda(hashl dict)
          (if(null? hashl)
             '()
             (cons (genbitvector (car hashl)  dict) (combinevector (cdr hashl) dict))
             )
          )
         ))
      (process x y)
      )
    )
  )

(define bitchecker
  (lambda (x y z)
    (letrec
        ((process
          (lambda (hashfl INPUT bitvec)
            (if(null? hashfl)
               '()
               (combine(list(bitlistcheck((car hashfl) INPUT) bitvec) (bitchecker (cdr hashfl) INPUT bitvec)))
               )
            )
          )
         )
      (process x y z)
      )
    )
  )

(define bitlistcheck
  (lambda (x y)
    (letrec
      ((process
        (lambda (key bitvec)
          (if(null? bitvec)
             (list 0)
             (if (= key 0)
                 (list 0)
                 (if (= key (car bitvec))
                 (list 1)
                 (process key (cdr bitvec))
                 ))
             )
          )
        )
       )
    (process x y)
      )
    )
  )

;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
    (if(null? w)
       0
       (letrec
           ((process
            (lambda(INPUT)
              (if(null? INPUT)
                 5187
                 (+ (ctv (car INPUT)) ( * 29 (process (cdr INPUT))))
                 )
              )
            ))
         (process w)
         )
       )
    )
  )


;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 106402241991
;;   (key '(m a y))           = 126526810
;;   (key '(t r e e f r o g)) = 2594908189083745

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (INPUT)
      (modulo (key INPUT) size)
      )
    )
  )

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17
(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (INPUT)
      (letrec
          ((x (- (* (key INPUT) A) (floor(* (key INPUT) A))) ))
        (floor (* size x))
        )
      )
    )
  )

;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 7224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))       ==> 35616
;;  (hash-1 '(m a y))           ==> 46566
;;  (hash-1 '(t r e e f r o g)) ==> 48238
;;
;;  (hash-2 '(h e l l o))       ==> 48849
;;  (hash-2 '(m a y))           ==> 81025
;;  (hash-2 '(t r e e f r o g)) ==> 16708
;;
;;  (hash-3 '(h e l l o))       ==> 6331.0
;;  (hash-3 '(m a y))           ==> 2456.0
;;  (hash-3 '(t r e e f r o g)) ==> 1806.0
;;
;;  (hash-4 '(h e l l o))       ==> 788.0
;;  (hash-4 '(m a y))           ==> 306.0
;;  (hash-4 '(t r e e f r o g)) ==> 225.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda ( hashfunctionlist dict)
    (lambda (INPUT)
      (let
          ((a (combine(combinevector hashfunctionlist dict))))
        (if(null? hashfunctionlist)
           '()
           (equal? (reduce * (bitchecker hashfunctionlist INPUT a) 1) 1)
           )
        )
      )
    )
  )

;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #t  // false positive

